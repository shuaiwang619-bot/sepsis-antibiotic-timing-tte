# eICU validation t0 supplement audit
# Scope:
# 1) Hospital-level microLab availability, to separate upload missingness from
#    patient-level culture absence.
# 2) ICU-admission anchored pre-t0 antibiotic alignment checks.

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

psql_exe <- Sys.getenv("PSQL_EXE", "D:/Physionet/postgresql/pgsql/bin/psql.exe")
pg_host <- Sys.getenv("PGHOST", "127.0.0.1")
pg_port <- Sys.getenv("PGPORT", "5431")
pg_user <- Sys.getenv("PGUSER", "postgres")
pg_db <- Sys.getenv("PGDATABASE", "eicu")

if (!file.exists(psql_exe)) {
  stop("psql executable not found. Set PSQL_EXE or check: ", psql_exe, call. = FALSE)
}

dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

out_hospital_summary <- file.path(base_dir, "eicu_v03b_microlab_hospital_coverage_summary.csv")
out_hospital_detail <- file.path(base_dir, "eicu_v03b_microlab_hospital_coverage_detail.csv")
out_pret0_summary <- file.path(base_dir, "eicu_v03b_pret0_abx_alignment_summary.csv")
out_pret0_windows <- file.path(base_dir, "eicu_v03b_pret0_abx_window_distribution.csv")
out_pret0_offsets <- file.path(base_dir, "eicu_v03b_pret0_abx_offset_summary.csv")
out_pret0_offset_bins <- file.path(base_dir, "eicu_v03b_pret0_abx_offset_bins.csv")
out_pret0_drugs <- file.path(base_dir, "eicu_v03b_pret0_abx_top_drugs.csv")
out_report <- file.path(base_dir, "eicu_v03b_t0_supplement_audit_report.txt")

sepsis_regex <- "\\m(sepsis|septic|septicemia|urosepsis)\\M"
explicit_sepsis_exclusion_regex <- "(signs and symptoms of sepsis|\\mSIRS\\M)"

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
po_or_local_regex <- "\\m(PO|ORAL|TAB|TABS|TABLET|TABLETS|CAP|CAPS|CAPSULE|CAPSULES|SUSP|SUSPENSION|ELIXIR|OPHTH|OPHTHALMIC|OTIC|TOPICAL|CREAM|OINT|OINTMENT|NEB|NEBULIZER|INHAL|INHALATION|MOUTH|IRRIG|IRRIGATION|LOCK)\\M"

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
    diagnosisstring::text as label_text
  from eicu_crd.diagnosis
  where diagnosisstring ~* %s

  union all

  select
    patientunitstayid,
    admitdxenteredoffset as label_offset_min,
    concat_ws(' | ', admitdxpath, admitdxname, admitdxtext)::text as label_text
  from eicu_crd.admissiondx
  where admitdxpath ~* %s
     or coalesce(admitdxname, '') ~* %s
     or coalesce(admitdxtext, '') ~* %s
),
strict_sepsis as (
  select patientunitstayid
  from sepsis_label_raw
  group by patientunitstayid
  having bool_or(label_text ~* %s and label_text !~* %s)
),
abx_raw as (
  select
    patientunitstayid,
    drugstartoffset as abx_offset_min,
    drugname::text as drugname,
    'medication'::text as abx_source
  from eicu_crd.medication
  where drugname ~* %s
    and drugname !~* %s
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
    and drugname !~* %s
    and infusionoffset is not null
),
first_abx as (
  select distinct on (patientunitstayid)
    patientunitstayid,
    abx_offset_min as first_abx_offset_min,
    drugname as first_abx_drugname,
    abx_source as first_abx_source
  from abx_raw
  order by patientunitstayid, abx_offset_min, abx_source, drugname
),
cohort_all as (
  select
    p.patientunitstayid,
    p.uniquepid,
    p.hospitalid,
    case
      when p.age ~ '^[0-9]+$' then p.age::int
      when p.age = '> 89' then 90
      else null
    end as age_num,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,
    f.first_abx_offset_min,
    f.first_abx_drugname,
    f.first_abx_source,
    case
      when lower(coalesce(p.hospitaldischargestatus, '')) = 'expired'
           and p.hospitaldischargeoffset <= 43200 then 1
      when p.hospitaldischargestatus is null or p.hospitaldischargestatus = '' then null
      else 0
    end as death_30d_inhosp,
    row_number() over (
      partition by coalesce(nullif(p.uniquepid, ''), p.patientunitstayid::text)
      order by p.patientunitstayid
    ) as rn_patient
  from strict_sepsis s
  join eicu_crd.patient p using (patientunitstayid)
  left join first_abx f using (patientunitstayid)
  where
    case
      when p.age ~ '^[0-9]+$' then p.age::int
      when p.age = '> 89' then 90
      else null
    end >= 18
),
cohort as (
  select *
  from cohort_all
  where rn_patient = 1
),
culture_events as (
  select
    patientunitstayid,
    culturetakenoffset as culture_offset_min,
    culturesite,
    count(*) as microlab_rows_for_event
  from eicu_crd.microlab
  where culturetakenoffset is not null
  group by patientunitstayid, culturetakenoffset, culturesite
),
culture_flags as (
  select
    patientunitstayid,
    1 as any_valid_culture_offset,
    max((culture_offset_min between -1440 and 4320)::int) as culture_m24_p72,
    max((culture_offset_min between -1440 and 1440)::int) as culture_m24_p24,
    max((culture_offset_min between -360 and 1440)::int) as culture_m6_p24,
    max((culture_offset_min between 0 and 4320)::int) as culture_0_p72
  from culture_events
  group by patientunitstayid
),
hospital_any_microlab_row as (
  select
    p.hospitalid,
    count(*) as microlab_rows_all_eicu,
    count(distinct ml.patientunitstayid) as microlab_stays_all_eicu
  from eicu_crd.microlab ml
  join eicu_crd.patient p using (patientunitstayid)
  group by p.hospitalid
),
hospital_valid_microlab_offset as (
  select
    p.hospitalid,
    count(*) as valid_offset_microlab_rows_all_eicu,
    count(distinct ml.patientunitstayid) as valid_offset_microlab_stays_all_eicu
  from eicu_crd.microlab ml
  join eicu_crd.patient p using (patientunitstayid)
  where ml.culturetakenoffset is not null
  group by p.hospitalid
),
unit_classified as (
  select
    c.*,
    case
      when first_abx_offset_min is null then 'no_abx_record'
      when first_abx_offset_min < 0 then 'pre_t0_abx'
      when first_abx_offset_min <= 180 then '0-3h'
      when first_abx_offset_min <= 360 then '3-6h'
      when first_abx_offset_min <= 720 then '6-12h'
      when first_abx_offset_min <= 4320 then '>12-72h'
      else '>72h'
    end as abx_window_unit_t0
  from cohort c
)
",
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(explicit_sepsis_exclusion_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(po_or_local_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(po_or_local_regex)
)

hospital_summary_sql <- paste0(base_cte, "
select
  (select count(*) from cohort) as cohort_n,
  (select count(distinct hospitalid) from cohort) as cohort_hospitals,
  (select count(distinct hospitalid) from eicu_crd.patient) as all_eicu_hospitals,
  (select count(distinct hospitalid) from hospital_any_microlab_row) as all_eicu_hospitals_with_any_microlab_row,
  round(100.0 * (select count(distinct hospitalid) from hospital_any_microlab_row) /
        nullif((select count(distinct hospitalid) from eicu_crd.patient), 0), 2) as all_eicu_hospitals_with_any_microlab_row_pct,
  (select count(distinct hospitalid) from hospital_valid_microlab_offset) as all_eicu_hospitals_with_valid_culture_offset,
  round(100.0 * (select count(distinct hospitalid) from hospital_valid_microlab_offset) /
        nullif((select count(distinct hospitalid) from eicu_crd.patient), 0), 2) as all_eicu_hospitals_with_valid_culture_offset_pct,
  count(distinct c.hospitalid) filter (where har.hospitalid is not null) as cohort_hospitals_with_any_microlab_row,
  round(100.0 * count(distinct c.hospitalid) filter (where har.hospitalid is not null) /
        nullif(count(distinct c.hospitalid), 0), 2) as cohort_hospitals_with_any_microlab_row_pct,
  count(distinct c.hospitalid) filter (where hvo.hospitalid is not null) as cohort_hospitals_with_valid_culture_offset,
  round(100.0 * count(distinct c.hospitalid) filter (where hvo.hospitalid is not null) /
        nullif(count(distinct c.hospitalid), 0), 2) as cohort_hospitals_with_valid_culture_offset_pct,
  count(*) filter (where hvo.hospitalid is not null) as cohort_stays_in_valid_reporting_hospitals,
  round(100.0 * count(*) filter (where hvo.hospitalid is not null) / nullif(count(*), 0), 2) as cohort_stays_in_valid_reporting_hospitals_pct,
  count(*) filter (where hvo.hospitalid is null) as cohort_stays_in_non_valid_reporting_hospitals,
  round(100.0 * count(*) filter (where hvo.hospitalid is null) / nullif(count(*), 0), 2) as cohort_stays_in_non_valid_reporting_hospitals_pct,
  sum(coalesce(cf.any_valid_culture_offset, 0)) as any_culture_stays_all_cohort,
  round(100.0 * sum(coalesce(cf.any_valid_culture_offset, 0)) / nullif(count(*), 0), 2) as any_culture_stays_all_cohort_pct,
  sum(coalesce(cf.any_valid_culture_offset, 0)) filter (where hvo.hospitalid is not null) as any_culture_stays_in_valid_reporting_hospitals,
  round(100.0 * sum(coalesce(cf.any_valid_culture_offset, 0)) filter (where hvo.hospitalid is not null) /
        nullif(count(*) filter (where hvo.hospitalid is not null), 0), 2) as any_culture_stays_in_valid_reporting_hospitals_pct,
  sum(coalesce(cf.culture_m24_p72, 0)) as culture_m24_p72_stays_all_cohort,
  round(100.0 * sum(coalesce(cf.culture_m24_p72, 0)) / nullif(count(*), 0), 2) as culture_m24_p72_stays_all_cohort_pct,
  sum(coalesce(cf.culture_m24_p72, 0)) filter (where hvo.hospitalid is not null) as culture_m24_p72_stays_in_valid_reporting_hospitals,
  round(100.0 * sum(coalesce(cf.culture_m24_p72, 0)) filter (where hvo.hospitalid is not null) /
        nullif(count(*) filter (where hvo.hospitalid is not null), 0), 2) as culture_m24_p72_stays_in_valid_reporting_hospitals_pct
from cohort c
left join culture_flags cf using (patientunitstayid)
left join hospital_any_microlab_row har using (hospitalid)
left join hospital_valid_microlab_offset hvo using (hospitalid)
")

hospital_detail_sql <- paste0(base_cte, "
select
  c.hospitalid,
  count(*) as cohort_stays,
  max((har.hospitalid is not null)::int) as hospital_has_any_microlab_row_all_eicu,
  max((hvo.hospitalid is not null)::int) as hospital_has_valid_culture_offset_all_eicu,
  coalesce(max(har.microlab_stays_all_eicu), 0) as microlab_stays_all_eicu,
  coalesce(max(hvo.valid_offset_microlab_stays_all_eicu), 0) as valid_offset_microlab_stays_all_eicu,
  sum(coalesce(cf.any_valid_culture_offset, 0)) as cohort_any_culture_stays,
  round(100.0 * sum(coalesce(cf.any_valid_culture_offset, 0)) / nullif(count(*), 0), 2) as cohort_any_culture_stays_pct,
  sum(coalesce(cf.culture_m24_p72, 0)) as cohort_culture_m24_p72_stays,
  round(100.0 * sum(coalesce(cf.culture_m24_p72, 0)) / nullif(count(*), 0), 2) as cohort_culture_m24_p72_stays_pct
from cohort c
left join culture_flags cf using (patientunitstayid)
left join hospital_any_microlab_row har using (hospitalid)
left join hospital_valid_microlab_offset hvo using (hospitalid)
group by c.hospitalid
order by cohort_stays desc, c.hospitalid
")

pret0_summary_sql <- paste0(base_cte, "
select
  count(*) as cohort_n,
  count(distinct patientunitstayid) as distinct_stays,
  count(*) - count(distinct patientunitstayid) as duplicate_stay_rows,
  sum((abx_window_unit_t0 is not null)::int) as assigned_window_rows,
  sum((abx_window_unit_t0 = 'pre_t0_abx')::int) as pre_t0_abx_n,
  sum((abx_window_unit_t0 = 'no_abx_record')::int) as no_abx_record_n,
  sum((abx_window_unit_t0 in ('0-3h','3-6h','6-12h'))::int) as three_window_candidate_n,
  sum((abx_window_unit_t0 = '0-3h')::int) as window_0_3h_n,
  sum((abx_window_unit_t0 = '3-6h')::int) as window_3_6h_n,
  sum((abx_window_unit_t0 = '6-12h')::int) as window_6_12h_n,
  sum((abx_window_unit_t0 = '>12-72h')::int) as window_gt12_72h_n,
  sum((abx_window_unit_t0 = '>72h')::int) as window_gt72h_n,
  sum((first_abx_offset_min < 0 and abx_window_unit_t0 <> 'pre_t0_abx')::int) as negative_offset_not_pre_t0_n,
  sum((first_abx_offset_min >= 0 and abx_window_unit_t0 = 'pre_t0_abx')::int) as nonnegative_offset_marked_pre_t0_n,
  sum((first_abx_offset_min is null and abx_window_unit_t0 <> 'no_abx_record')::int) as null_abx_not_no_record_n,
  sum((first_abx_offset_min is not null and abx_window_unit_t0 = 'no_abx_record')::int) as nonnull_abx_marked_no_record_n,
  sum((first_abx_offset_min < 0 and abx_window_unit_t0 in ('0-3h','3-6h','6-12h','>12-72h','>72h'))::int) as negative_offset_in_post_t0_windows_n,
  sum((abx_window_unit_t0 in ('pre_t0_abx','no_abx_record','0-3h','3-6h','6-12h','>12-72h','>72h'))::int) as category_sum_n
from unit_classified
")

pret0_windows_sql <- paste0(base_cte, "
select
  abx_window_unit_t0,
  count(*) as stays,
  round(100.0 * count(*) / sum(count(*)) over (), 2) as pct_of_cohort,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct
from unit_classified
group by abx_window_unit_t0
order by
  case abx_window_unit_t0
    when 'pre_t0_abx' then 1
    when '0-3h' then 2
    when '3-6h' then 3
    when '6-12h' then 4
    when '>12-72h' then 5
    when '>72h' then 6
    when 'no_abx_record' then 7
    else 99
  end
")

pret0_offsets_sql <- paste0(base_cte, "
select
  count(*) as pre_t0_abx_n,
  round(min(first_abx_offset_min::numeric / 60.0), 3) as min_pre_t0_offset_h,
  round(percentile_cont(0.25) within group (order by first_abx_offset_min::numeric / 60.0)::numeric, 3) as p25_pre_t0_offset_h,
  round(percentile_cont(0.5) within group (order by first_abx_offset_min::numeric / 60.0)::numeric, 3) as median_pre_t0_offset_h,
  round(percentile_cont(0.75) within group (order by first_abx_offset_min::numeric / 60.0)::numeric, 3) as p75_pre_t0_offset_h,
  round(max(first_abx_offset_min::numeric / 60.0), 3) as max_pre_t0_offset_h
from unit_classified
where abx_window_unit_t0 = 'pre_t0_abx'
")

pret0_offset_bins_sql <- paste0(base_cte, "
select
  pre_t0_offset_bin,
  count(*) as pre_t0_stays,
  round(100.0 * count(*) / sum(count(*)) over (), 2) as pct_of_pre_t0,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct
from (
  select
    death_30d_inhosp,
    case
      when first_abx_offset_min < -4320 then '<-72h'
      when first_abx_offset_min < -1440 then '-72h to -24h'
      when first_abx_offset_min < -720 then '-24h to -12h'
      when first_abx_offset_min < -360 then '-12h to -6h'
      when first_abx_offset_min < -180 then '-6h to -3h'
      else '-3h to 0h'
    end as pre_t0_offset_bin,
    case
      when first_abx_offset_min < -4320 then 1
      when first_abx_offset_min < -1440 then 2
      when first_abx_offset_min < -720 then 3
      when first_abx_offset_min < -360 then 4
      when first_abx_offset_min < -180 then 5
      else 6
    end as bin_order
  from unit_classified
  where abx_window_unit_t0 = 'pre_t0_abx'
) x
group by pre_t0_offset_bin, bin_order
order by bin_order
")

pret0_drugs_sql <- paste0(base_cte, "
select
  first_abx_source,
  first_abx_drugname,
  count(*) as pre_t0_stays,
  round(100.0 * count(*) / sum(count(*)) over (), 2) as pct_of_pre_t0,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct
from unit_classified
where abx_window_unit_t0 = 'pre_t0_abx'
group by first_abx_source, first_abx_drugname
order by pre_t0_stays desc, first_abx_source, first_abx_drugname
limit 50
")

print_section("Run hospital-level microLab coverage")
hospital_summary <- run_query(hospital_summary_sql)
hospital_detail <- run_query(hospital_detail_sql)

print_section("Run pre-t0 antibiotic alignment")
pret0_summary <- run_query(pret0_summary_sql)
pret0_windows <- run_query(pret0_windows_sql)
pret0_offsets <- run_query(pret0_offsets_sql)
pret0_offset_bins <- run_query(pret0_offset_bins_sql)
pret0_drugs <- run_query(pret0_drugs_sql)

setDT(hospital_summary)
setDT(hospital_detail)
setDT(pret0_summary)
setDT(pret0_windows)
setDT(pret0_offsets)
setDT(pret0_offset_bins)
setDT(pret0_drugs)

fwrite(hospital_summary, out_hospital_summary)
fwrite(hospital_detail, out_hospital_detail)
fwrite(pret0_summary, out_pret0_summary)
fwrite(pret0_windows, out_pret0_windows)
fwrite(pret0_offsets, out_pret0_offsets)
fwrite(pret0_offset_bins, out_pret0_offset_bins)
fwrite(pret0_drugs, out_pret0_drugs)

report_lines <- c(
  "eICU t0 supplement audit",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Database: ", pg_db, " / schema eicu_crd"),
  paste0("Base directory: ", base_dir),
  "",
  "Cohort:",
  "- strict explicit sepsis label",
  "- adult",
  "- first qualifying stay per uniquepid",
  "- antibiotics exclude non-administration terms and PO/local route terms",
  "",
  "Hospital-level microLab coverage summary:",
  paste(capture.output(print(hospital_summary)), collapse = "\n"),
  "",
  "Pre-t0 antibiotic alignment summary:",
  paste(capture.output(print(pret0_summary)), collapse = "\n"),
  "",
  "ICU-admission anchored antibiotic window distribution:",
  paste(capture.output(print(pret0_windows)), collapse = "\n"),
  "",
  "Pre-t0 antibiotic offset summary, hours relative to ICU admission:",
  paste(capture.output(print(pret0_offsets)), collapse = "\n"),
  "",
  "Pre-t0 antibiotic offset bins:",
  paste(capture.output(print(pret0_offset_bins)), collapse = "\n"),
  "",
  "Top pre-t0 antibiotic records:",
  paste(capture.output(print(head(pret0_drugs, 20))), collapse = "\n")
)

writeLines(report_lines, out_report, useBytes = TRUE)

print_section("Done")
cat("Wrote:\n")
cat(" - ", out_hospital_summary, "\n", sep = "")
cat(" - ", out_hospital_detail, "\n", sep = "")
cat(" - ", out_pret0_summary, "\n", sep = "")
cat(" - ", out_pret0_windows, "\n", sep = "")
cat(" - ", out_pret0_offsets, "\n", sep = "")
cat(" - ", out_pret0_offset_bins, "\n", sep = "")
cat(" - ", out_pret0_drugs, "\n", sep = "")
cat(" - ", out_report, "\n", sep = "")
