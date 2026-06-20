# eICU validation cohort and antibiotic timing audit
# Scope: build a first-pass sepsis-label cohort, first antibiotic offset,
# exposure-window distribution, and outcome availability audit.
# This script does not fit models and does not change MIMIC formal_v1 outputs.

set.seed(2025)

get_script_dir <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }
  getwd()
}

script_dir <- get_script_dir()
default_base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)

args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else default_base_dir

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

library(data.table)

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

sql_literal <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

normalize_empty <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

yes_no <- function(x) {
  ifelse(is.na(x), NA_integer_, as.integer(x))
}

psql_exe <- Sys.getenv("PSQL_EXE", "D:/Physionet/postgresql/pgsql/bin/psql.exe")
pg_host <- Sys.getenv("PGHOST", "127.0.0.1")
pg_port <- Sys.getenv("PGPORT", "5431")
pg_user <- Sys.getenv("PGUSER", "postgres")
pg_db <- Sys.getenv("PGDATABASE", "eicu")

if (!file.exists(psql_exe)) {
  stop("psql executable not found. Set PSQL_EXE or check: ", psql_exe, call. = FALSE)
}

if (!nzchar(Sys.getenv("PGPASSWORD"))) {
  warning("PGPASSWORD is empty. If PostgreSQL requires a password, set Sys.setenv(PGPASSWORD='...') before running.")
}

dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

out_cohort <- file.path(base_dir, "eicu_v01_validation_cohort_abx_audit.csv")
out_summary <- file.path(base_dir, "eicu_v01_validation_audit_summary.csv")
out_window <- file.path(base_dir, "eicu_v01_validation_abx_window_distribution.csv")
out_drugs <- file.path(base_dir, "eicu_v01_validation_antibiotic_candidate_terms.csv")
out_sepsis_labels <- file.path(base_dir, "eicu_v01_validation_sepsis_label_terms.csv")
out_report <- file.path(base_dir, "eicu_v01_validation_cohort_abx_audit_report.txt")

sepsis_regex <- "\\m(sepsis|septic|septicemia|urosepsis)\\M"

abx_regex <- paste(
  c(
    "\\mvancomycin\\M", "\\mvancocin\\M", "\\mvanco\\M",
    "\\mcefepime\\M", "\\mceftriaxone\\M", "\\mceftazidime\\M", "\\mcefazolin\\M",
    "\\mpiperacillin\\M", "\\mtazobactam\\M", "\\mzosyn\\M",
    "\\mmeropenem\\M", "\\mimipenem\\M", "\\mertapenem\\M",
    "\\mazithromycin\\M",
    "\\mlevofloxacin\\M", "\\mlevaquin\\M", "\\mciprofloxacin\\M", "\\mmoxifloxacin\\M",
    "\\mampicillin\\M", "\\munasyn\\M",
    "\\mgentamicin\\M", "\\mtobramycin\\M", "\\mamikacin\\M",
    "\\mmetronidazole\\M", "\\mflagyl\\M",
    "\\mlinezolid\\M", "\\mdaptomycin\\M", "\\mclindamycin\\M", "\\mdoxycycline\\M"
  ),
  collapse = "|"
)

non_admin_drug_regex <- "\\m(consult|pharmacy|trough|level|random|peak|placeholder|protocol|monitor)\\M"

run_query <- function(sql) {
  sql <- sub(";\\s*$", "", sql)
  copy_sql <- paste0("copy (", sql, ") to stdout with csv header")
  sql_file <- tempfile(fileext = ".sql")
  writeLines(paste0(copy_sql, ";"), sql_file, useBytes = TRUE)
  on.exit(unlink(sql_file), add = TRUE)
  res <- system2(
    psql_exe,
    args = c(
      "--no-psqlrc",
      "-h", pg_host,
      "-p", pg_port,
      "-U", pg_user,
      "-d", pg_db,
      "-v", "ON_ERROR_STOP=1",
      "-P", "pager=off",
      "-q",
      "-f", sql_file
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    cat(paste(res, collapse = "\n"), "\n")
    stop("psql query failed with status ", status, call. = FALSE)
  }
  data.table::fread(text = paste(res, collapse = "\n"), showProgress = FALSE)
}

base_cte <- sprintf("
with sepsis_label_raw as (
  select
    patientunitstayid,
    diagnosisoffset as label_offset_min,
    'diagnosis'::text as label_source,
    diagnosisstring::text as label_text
  from eicu_crd.diagnosis
  where diagnosisstring ~* %s

  union all

  select
    patientunitstayid,
    admitdxenteredoffset as label_offset_min,
    'admissiondx'::text as label_source,
    concat_ws(' | ', admitdxpath, admitdxname, admitdxtext)::text as label_text
  from eicu_crd.admissiondx
  where admitdxpath ~* %s
     or coalesce(admitdxname, '') ~* %s
     or coalesce(admitdxtext, '') ~* %s
),
sepsis_dx as (
  select
    patientunitstayid,
    min(label_offset_min) as sepsis_label_first_offset_min,
    count(*) as sepsis_label_rows,
    bool_or(label_source = 'diagnosis') as has_diagnosis_sepsis,
    bool_or(label_source = 'admissiondx') as has_admissiondx_sepsis
  from sepsis_label_raw
  group by patientunitstayid
),
abx_all as (
  select
    patientunitstayid,
    drugstartoffset as abx_offset_min,
    drugname::text as drugname,
    'medication'::text as abx_source
  from eicu_crd.medication
  where drugname ~* %s
    and drugname !~* %s
    and coalesce(drugordercancelled, '') !~* 'yes|true|cancel'
    and drugstartoffset is not null

  union all

  select
    patientunitstayid,
    infusionoffset as abx_offset_min,
    drugname::text as drugname,
    'infusiondrug'::text as abx_source
  from eicu_crd.infusiondrug
  where drugname ~* %s
    and drugname !~* %s
    and infusionoffset is not null
),
first_abx as (
  select distinct on (patientunitstayid)
    patientunitstayid,
    abx_offset_min as first_abx_offset_min,
    drugname as first_abx_drugname,
    abx_source as first_abx_source
  from abx_all
  order by patientunitstayid, abx_offset_min, abx_source, drugname
),
cohort_all as (
  select
    p.patientunitstayid,
    p.uniquepid,
    p.hospitalid,
    p.gender,
    p.age as age_raw,
    case
      when p.age ~ '^[0-9]+$' then p.age::int
      when p.age = '> 89' then 90
      else null
    end as age_num,
    p.ethnicity,
    p.hospitaladmitsource,
    p.unitadmitsource,
    p.unitvisitnumber,
    p.unitstaytype,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,
    p.unitdischargeoffset,
    p.unitdischargestatus,
    s.sepsis_label_first_offset_min,
    s.sepsis_label_rows,
    s.has_diagnosis_sepsis,
    s.has_admissiondx_sepsis,
    f.first_abx_offset_min,
    round((f.first_abx_offset_min::numeric / 60.0), 3) as first_abx_delay_hours_from_unit,
    f.first_abx_drugname,
    f.first_abx_source,
    case
      when f.first_abx_offset_min is null then 'no_abx_record'
      when f.first_abx_offset_min < 0 then 'pre_unit_admit'
      when f.first_abx_offset_min <= 180 then '0-3h'
      when f.first_abx_offset_min <= 360 then '3-6h'
      when f.first_abx_offset_min <= 720 then '6-12h'
      when f.first_abx_offset_min <= 4320 then '>=12-72h'
      else '>72h'
    end as abx_window_eicu,
    case
      when lower(coalesce(p.hospitaldischargestatus, '')) = 'expired'
           and p.hospitaldischargeoffset <= 43200 then 1
      when p.hospitaldischargestatus is null or p.hospitaldischargestatus = '' then null
      else 0
    end as death_30d_inhosp,
    case
      when lower(coalesce(p.hospitaldischargestatus, '')) = 'expired' then 1
      when p.hospitaldischargestatus is null or p.hospitaldischargestatus = '' then null
      else 0
    end as death_hosp,
    case
      when lower(coalesce(p.unitdischargestatus, '')) = 'expired' then 1
      when p.unitdischargestatus is null or p.unitdischargestatus = '' then null
      else 0
    end as death_icu,
    (least(p.hospitaldischargeoffset, 43200)::numeric / 1440.0) as followup_30d_inhosp_days,
    row_number() over (
      partition by coalesce(nullif(p.uniquepid, ''), p.patientunitstayid::text)
      order by p.patientunitstayid
    ) as rn_patient
  from sepsis_dx s
  join eicu_crd.patient p using (patientunitstayid)
  left join first_abx f using (patientunitstayid)
)
",
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex)
)

cohort_sql <- paste0(base_cte, "
select *
from cohort_all
where age_num >= 18
order by patientunitstayid
")

drug_terms_sql <- sprintf("
with abx_all as (
  select
    patientunitstayid,
    drugstartoffset as abx_offset_min,
    drugname::text as drugname,
    'medication'::text as abx_source
  from eicu_crd.medication
  where drugname ~* %s
    and drugname !~* %s
    and coalesce(drugordercancelled, '') !~* 'yes|true|cancel'
    and drugstartoffset is not null

  union all

  select
    patientunitstayid,
    infusionoffset as abx_offset_min,
    drugname::text as drugname,
    'infusiondrug'::text as abx_source
  from eicu_crd.infusiondrug
  where drugname ~* %s
    and drugname !~* %s
    and infusionoffset is not null
)
select
  abx_source,
  drugname,
  count(*) as rows,
  count(distinct patientunitstayid) as stays,
  min(abx_offset_min) as min_offset_min,
  max(abx_offset_min) as max_offset_min
from abx_all
group by abx_source, drugname
order by stays desc, rows desc, drugname
limit 300
",
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex)
)

sepsis_terms_sql <- sprintf("
with sepsis_label_raw as (
  select
    patientunitstayid,
    diagnosisoffset as label_offset_min,
    'diagnosis'::text as label_source,
    diagnosisstring::text as label_text
  from eicu_crd.diagnosis
  where diagnosisstring ~* %s

  union all

  select
    patientunitstayid,
    admitdxenteredoffset as label_offset_min,
    'admissiondx'::text as label_source,
    concat_ws(' | ', admitdxpath, admitdxname, admitdxtext)::text as label_text
  from eicu_crd.admissiondx
  where admitdxpath ~* %s
     or coalesce(admitdxname, '') ~* %s
     or coalesce(admitdxtext, '') ~* %s
)
select
  label_source,
  label_text,
  count(*) as rows,
  count(distinct patientunitstayid) as stays,
  min(label_offset_min) as min_offset_min,
  max(label_offset_min) as max_offset_min
from sepsis_label_raw
group by label_source, label_text
order by stays desc, rows desc, label_text
limit 300
",
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex)
)

print_section("Extract eICU validation audit cohort")
cohort <- run_query(cohort_sql)
data.table::setDT(cohort)

cohort[, is_first_patient_stay := rn_patient == 1]
cohort[, main_three_window_candidate := abx_window_eicu %in% c("0-3h", "3-6h", "6-12h")]
cohort[, post_unit_abx_72h := abx_window_eicu %in% c("0-3h", "3-6h", "6-12h", ">=12-72h")]
cohort[, pre_unit_abx := abx_window_eicu == "pre_unit_admit"]
cohort[, no_abx_record := abx_window_eicu == "no_abx_record"]

window_dist <- cohort[
  ,
  .(
    stays = .N,
    death_30d_inhosp_n = sum(death_30d_inhosp == 1, na.rm = TRUE),
    death_30d_inhosp_pct = round(100 * mean(death_30d_inhosp == 1, na.rm = TRUE), 2),
    death_hosp_n = sum(death_hosp == 1, na.rm = TRUE),
    death_hosp_pct = round(100 * mean(death_hosp == 1, na.rm = TRUE), 2)
  ),
  by = .(abx_window_eicu)
]
window_dist[, cohort_scope := "all_adult_sepsis_label_stays"]
setcolorder(window_dist, c("cohort_scope", setdiff(names(window_dist), "cohort_scope")))

window_dist_first <- cohort[
  is_first_patient_stay == TRUE,
  .(
    stays = .N,
    death_30d_inhosp_n = sum(death_30d_inhosp == 1, na.rm = TRUE),
    death_30d_inhosp_pct = round(100 * mean(death_30d_inhosp == 1, na.rm = TRUE), 2),
    death_hosp_n = sum(death_hosp == 1, na.rm = TRUE),
    death_hosp_pct = round(100 * mean(death_hosp == 1, na.rm = TRUE), 2)
  ),
  by = .(abx_window_eicu)
]
window_dist_first[, cohort_scope := "first_qualifying_stay_per_uniquepid"]
setcolorder(window_dist_first, c("cohort_scope", setdiff(names(window_dist_first), "cohort_scope")))

window_dist <- rbindlist(list(window_dist, window_dist_first), use.names = TRUE)
window_levels <- c("pre_unit_admit", "0-3h", "3-6h", "6-12h", ">=12-72h", ">72h", "no_abx_record")
window_dist[, abx_window_eicu := factor(abx_window_eicu, levels = window_levels)]
setorder(window_dist, cohort_scope, abx_window_eicu)

summary_one <- function(dt, scope) {
  data.table(
    cohort_scope = scope,
    metric = c(
      "adult_sepsis_label_stays",
      "unique_pid_n",
      "hospital_n",
      "death_30d_inhosp_n",
      "death_hosp_n",
      "death_icu_n",
      "hospital_status_missing_n",
      "pre_unit_abx_n",
      "no_abx_record_n",
      "post_unit_abx_72h_n",
      "three_window_candidate_n",
      "window_0_3h_n",
      "window_3_6h_n",
      "window_6_12h_n",
      "window_ge12_72h_n",
      "window_gt72h_n"
    ),
    value = c(
      nrow(dt),
      uniqueN(dt$uniquepid),
      uniqueN(dt$hospitalid),
      sum(dt$death_30d_inhosp == 1, na.rm = TRUE),
      sum(dt$death_hosp == 1, na.rm = TRUE),
      sum(dt$death_icu == 1, na.rm = TRUE),
      sum(is.na(dt$death_hosp)),
      sum(dt$pre_unit_abx, na.rm = TRUE),
      sum(dt$no_abx_record, na.rm = TRUE),
      sum(dt$post_unit_abx_72h, na.rm = TRUE),
      sum(dt$main_three_window_candidate, na.rm = TRUE),
      sum(dt$abx_window_eicu == "0-3h", na.rm = TRUE),
      sum(dt$abx_window_eicu == "3-6h", na.rm = TRUE),
      sum(dt$abx_window_eicu == "6-12h", na.rm = TRUE),
      sum(dt$abx_window_eicu == ">=12-72h", na.rm = TRUE),
      sum(dt$abx_window_eicu == ">72h", na.rm = TRUE)
    )
  )
}

summary_tbl <- rbindlist(
  list(
    summary_one(cohort, "all_adult_sepsis_label_stays"),
    summary_one(cohort[is_first_patient_stay == TRUE], "first_qualifying_stay_per_uniquepid")
  ),
  use.names = TRUE
)

print_section("Extract antibiotic candidate drug terms")
drug_terms <- run_query(drug_terms_sql)
data.table::setDT(drug_terms)

print_section("Extract sepsis label terms")
sepsis_terms <- run_query(sepsis_terms_sql)
data.table::setDT(sepsis_terms)

data.table::fwrite(cohort, out_cohort)
data.table::fwrite(summary_tbl, out_summary)
data.table::fwrite(window_dist, out_window)
data.table::fwrite(drug_terms, out_drugs)
data.table::fwrite(sepsis_terms, out_sepsis_labels)

report_lines <- c(
  "eICU validation cohort and antibiotic timing audit",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Database: ", pg_db, " / schema eicu_crd"),
  paste0("Base directory: ", base_dir),
  "",
  "Scope:",
  "- Adult sepsis-label candidate ICU stays.",
  "- Index is ICU unit admission offset 0.",
  "- Main outcome audited here is 30-day in-hospital mortality.",
  "- This is an audit table, not the final model-ready table.",
  "",
  "Output files:",
  paste0("- ", out_cohort),
  paste0("- ", out_summary),
  paste0("- ", out_window),
  paste0("- ", out_drugs),
  paste0("- ", out_sepsis_labels),
  "",
  "Summary:",
  paste(capture.output(print(summary_tbl)), collapse = "\n"),
  "",
  "Window distribution:",
  paste(capture.output(print(window_dist)), collapse = "\n")
)

writeLines(report_lines, out_report, useBytes = TRUE)

print_section("Done")
cat("Wrote:\n")
cat(" - ", out_cohort, "\n", sep = "")
cat(" - ", out_summary, "\n", sep = "")
cat(" - ", out_window, "\n", sep = "")
cat(" - ", out_drugs, "\n", sep = "")
cat(" - ", out_sepsis_labels, "\n", sep = "")
cat(" - ", out_report, "\n", sep = "")
