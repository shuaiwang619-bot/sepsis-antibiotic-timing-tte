# eICU validation definition-scenario audit
# Scope: compare broad vs strict sepsis-label definitions and antibiotic
# route filters before any modeling.

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

out_summary <- file.path(base_dir, "eicu_v02_definition_scenario_summary.csv")
out_windows <- file.path(base_dir, "eicu_v02_definition_scenario_windows.csv")
out_abx_terms <- file.path(base_dir, "eicu_v02_antibiotic_term_review.csv")
out_sepsis_terms <- file.path(base_dir, "eicu_v02_sepsis_label_review.csv")
out_report <- file.path(base_dir, "eicu_v02_definition_scenarios_report.txt")

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
iv_explicit_regex <- "\\m(IV|IVPB|IJ|INJ|VIAL|D5W|NS|NACL|SODIUM CHLORIDE|MINI-BAG|SYRINGE|RTU-PB|PIGGYBACK|SOLR)\\M"

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

scenario_cte <- sprintf("
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
sepsis_stay_flags as (
  select
    patientunitstayid,
    bool_or(label_text ~* %s and label_text !~* %s) as has_explicit_sepsis,
    bool_or(label_text ~* %s) as has_broad_sepsis_label,
    bool_or(label_text ~* %s) as has_sirs_label
  from sepsis_label_raw
  group by patientunitstayid
),
sepsis_scope as (
  select patientunitstayid, 'broad_sepsis_or_sirs_label'::text as sepsis_definition
  from sepsis_stay_flags
  where has_broad_sepsis_label

  union all

  select patientunitstayid, 'strict_explicit_sepsis_label'::text as sepsis_definition
  from sepsis_stay_flags
  where has_explicit_sepsis
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
abx_flagged as (
  select
    *,
    (drugname ~* %s) as is_po_or_local,
    ((abx_source = 'infusiondrug') or drugname ~* %s) as is_iv_explicit
  from abx_raw
),
abx_by_definition as (
  select patientunitstayid, abx_offset_min, drugname, abx_source, 'all_candidate_nonadmin_excluded'::text as abx_definition
  from abx_flagged

  union all

  select patientunitstayid, abx_offset_min, drugname, abx_source, 'exclude_po_local'::text as abx_definition
  from abx_flagged
  where not is_po_or_local

  union all

  select patientunitstayid, abx_offset_min, drugname, abx_source, 'iv_explicit_only'::text as abx_definition
  from abx_flagged
  where is_iv_explicit and not is_po_or_local
),
first_abx as (
  select distinct on (abx_definition, patientunitstayid)
    abx_definition,
    patientunitstayid,
    abx_offset_min as first_abx_offset_min,
    drugname as first_abx_drugname,
    abx_source as first_abx_source
  from abx_by_definition
  order by abx_definition, patientunitstayid, abx_offset_min, abx_source, drugname
),
analysis_grid as (
  select
    ss.sepsis_definition,
    abx_def.abx_definition,
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
    p.unitdischargestatus,
    f.first_abx_offset_min,
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
    row_number() over (
      partition by ss.sepsis_definition, abx_def.abx_definition,
                   coalesce(nullif(p.uniquepid, ''), p.patientunitstayid::text)
      order by p.patientunitstayid
    ) as rn_patient
  from sepsis_scope ss
  join eicu_crd.patient p using (patientunitstayid)
  cross join (
    values
      ('all_candidate_nonadmin_excluded'::text),
      ('exclude_po_local'::text),
      ('iv_explicit_only'::text)
  ) as abx_def(abx_definition)
  left join first_abx f
    on f.patientunitstayid = ss.patientunitstayid
   and f.abx_definition = abx_def.abx_definition
  where
    case
      when p.age ~ '^[0-9]+$' then p.age::int
      when p.age = '> 89' then 90
      else null
    end >= 18
)
",
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(explicit_sepsis_exclusion_regex),
sql_literal(sepsis_regex),
sql_literal(explicit_sepsis_exclusion_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(po_or_local_regex),
sql_literal(iv_explicit_regex)
)

summary_sql <- paste0(scenario_cte, "
select
  sepsis_definition,
  abx_definition,
  'all_stays'::text as cohort_scope,
  count(*) as stays,
  count(distinct uniquepid) as unique_pid_n,
  count(distinct hospitalid) as hospital_n,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct,
  sum((death_hosp = 1)::int) as death_hosp_n,
  round(100.0 * avg((death_hosp = 1)::int), 2) as death_hosp_pct,
  sum((abx_window_eicu = 'pre_unit_admit')::int) as pre_unit_abx_n,
  sum((abx_window_eicu = 'no_abx_record')::int) as no_abx_record_n,
  sum((abx_window_eicu in ('0-3h','3-6h','6-12h'))::int) as three_window_candidate_n,
  sum((abx_window_eicu = '0-3h')::int) as window_0_3h_n,
  sum((abx_window_eicu = '3-6h')::int) as window_3_6h_n,
  sum((abx_window_eicu = '6-12h')::int) as window_6_12h_n,
  sum((abx_window_eicu = '>=12-72h')::int) as window_ge12_72h_n
from analysis_grid
group by sepsis_definition, abx_definition

union all

select
  sepsis_definition,
  abx_definition,
  'first_stay_per_uniquepid'::text as cohort_scope,
  count(*) as stays,
  count(distinct uniquepid) as unique_pid_n,
  count(distinct hospitalid) as hospital_n,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct,
  sum((death_hosp = 1)::int) as death_hosp_n,
  round(100.0 * avg((death_hosp = 1)::int), 2) as death_hosp_pct,
  sum((abx_window_eicu = 'pre_unit_admit')::int) as pre_unit_abx_n,
  sum((abx_window_eicu = 'no_abx_record')::int) as no_abx_record_n,
  sum((abx_window_eicu in ('0-3h','3-6h','6-12h'))::int) as three_window_candidate_n,
  sum((abx_window_eicu = '0-3h')::int) as window_0_3h_n,
  sum((abx_window_eicu = '3-6h')::int) as window_3_6h_n,
  sum((abx_window_eicu = '6-12h')::int) as window_6_12h_n,
  sum((abx_window_eicu = '>=12-72h')::int) as window_ge12_72h_n
from analysis_grid
where rn_patient = 1
group by sepsis_definition, abx_definition
order by sepsis_definition, abx_definition, cohort_scope
")

windows_sql <- paste0(scenario_cte, "
select
  sepsis_definition,
  abx_definition,
  'all_stays'::text as cohort_scope,
  abx_window_eicu,
  count(*) as stays,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct
from analysis_grid
group by sepsis_definition, abx_definition, abx_window_eicu

union all

select
  sepsis_definition,
  abx_definition,
  'first_stay_per_uniquepid'::text as cohort_scope,
  abx_window_eicu,
  count(*) as stays,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct
from analysis_grid
where rn_patient = 1
group by sepsis_definition, abx_definition, abx_window_eicu
order by sepsis_definition, abx_definition, cohort_scope, abx_window_eicu
")

abx_terms_sql <- sprintf("
with abx_raw as (
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
  max(abx_offset_min) as max_offset_min,
  (drugname ~* %s) as flag_po_or_local,
  ((abx_source = 'infusiondrug') or drugname ~* %s) as flag_iv_explicit,
  case
    when drugname ~* %s then 'exclude_from_primary_route_filter'
    when ((abx_source = 'infusiondrug') or drugname ~* %s) then 'primary_iv_explicit'
    else 'systemic_route_unclear_review'
  end as suggested_route_role
from abx_raw
group by abx_source, drugname
order by flag_po_or_local desc, suggested_route_role, stays desc, rows desc, drugname
",
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(po_or_local_regex),
sql_literal(iv_explicit_regex),
sql_literal(po_or_local_regex),
sql_literal(iv_explicit_regex)
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
  max(label_offset_min) as max_offset_min,
  (label_text !~* %s) as flag_explicit_sepsis_primary,
  case
    when label_text !~* %s then 'primary_explicit_sepsis'
    when label_text ~* '(infectious process with organ dysfunction)' then 'sirs_infectious_organ_dysfunction_sensitivity'
    else 'broad_only_review_or_exclude'
  end as suggested_sepsis_role
from sepsis_label_raw
group by label_source, label_text
order by flag_explicit_sepsis_primary, stays desc, rows desc, label_text
",
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(explicit_sepsis_exclusion_regex),
sql_literal(explicit_sepsis_exclusion_regex)
)

print_section("Run definition scenario summary")
summary_tbl <- run_query(summary_sql)
setDT(summary_tbl)

print_section("Run definition scenario windows")
windows_tbl <- run_query(windows_sql)
setDT(windows_tbl)

print_section("Run antibiotic term review")
abx_terms <- run_query(abx_terms_sql)
setDT(abx_terms)

print_section("Run sepsis label review")
sepsis_terms <- run_query(sepsis_terms_sql)
setDT(sepsis_terms)

fwrite(summary_tbl, out_summary)
fwrite(windows_tbl, out_windows)
fwrite(abx_terms, out_abx_terms)
fwrite(sepsis_terms, out_sepsis_terms)

focus <- summary_tbl[
  sepsis_definition == "strict_explicit_sepsis_label" &
    abx_definition == "exclude_po_local" &
    cohort_scope == "first_stay_per_uniquepid"
]

report_lines <- c(
  "eICU validation definition-scenario audit",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Database: ", pg_db, " / schema eicu_crd"),
  paste0("Base directory: ", base_dir),
  "",
  "Scenario definitions:",
  "- broad_sepsis_or_sirs_label: any sepsis/septic/septicemia/urosepsis label.",
  "- strict_explicit_sepsis_label: broad label excluding SIRS/signs-and-symptoms labels.",
  "- all_candidate_nonadmin_excluded: antibiotic terms after excluding consult/pharmacy/level-style non-administration terms.",
  "- exclude_po_local: additionally excludes PO/oral/topical/ophthalmic/otic/nebulized/irrigation/lock-style terms.",
  "- iv_explicit_only: only explicit IV/parenteral-looking terms plus infusiondrug records.",
  "",
  "Recommended first-pass validation scenario:",
  paste(capture.output(print(focus)), collapse = "\n"),
  "",
  "Full scenario summary:",
  paste(capture.output(print(summary_tbl)), collapse = "\n")
)

writeLines(report_lines, out_report, useBytes = TRUE)

print_section("Done")
cat("Wrote:\n")
cat(" - ", out_summary, "\n", sep = "")
cat(" - ", out_windows, "\n", sep = "")
cat(" - ", out_abx_terms, "\n", sep = "")
cat(" - ", out_sepsis_terms, "\n", sep = "")
cat(" - ", out_report, "\n", sep = "")
