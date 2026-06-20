# eICU formal_v1 step 01: full pipeline source tables
# MIMIC counterpart: 01_abx_tte_full_pipeline_formal_v1_20260612.sql
# Stage: wide table source
# Design: use the reviewed broad-sepsis audit cache as an ID prefilter, then run
# small chunked SQL pulls for strict labels, antibiotics, patient rows, and APACHE.

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
base_dir <- if (length(commandArgs(trailingOnly = TRUE)) >= 1) {
  commandArgs(trailingOnly = TRUE)[1]
} else {
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
}
out_dir <- file.path(base_dir, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

psql_exe <- Sys.getenv("PSQL_EXE", "D:/Physionet/postgresql/pgsql/bin/psql.exe")
pg_host <- Sys.getenv("PGHOST", "127.0.0.1")
pg_port <- Sys.getenv("PGPORT", "5431")
pg_user <- Sys.getenv("PGUSER", "postgres")
pg_db <- Sys.getenv("PGDATABASE", "eicu")
query_timeout_sec <- as.integer(Sys.getenv("EICU_FORMAL_STEP_TIMEOUT_SEC", "120"))
chunk_size <- as.integer(Sys.getenv("EICU_FORMAL_CHUNK_SIZE", "2000"))

if (!file.exists(psql_exe)) {
  stop("psql executable not found. Set PSQL_EXE or check: ", psql_exe, call. = FALSE)
}

out_sepsis_labels <- file.path(out_dir, "eicu_formal_v1_source_sepsis_label_rows.csv")
out_strict_sepsis <- file.path(out_dir, "eicu_formal_v1_source_strict_sepsis_stays.csv")
out_abx_rows <- file.path(out_dir, "eicu_formal_v1_source_antibiotic_candidate_rows.csv")
out_first_abx <- file.path(out_dir, "eicu_formal_v1_source_first_antibiotic.csv")
out_cohort <- file.path(out_dir, "eicu_formal_v1_source_cohort_exposure_outcome.csv")
out_apr <- file.path(out_dir, "eicu_formal_v1_source_apache_iva.csv")
out_aps <- file.path(out_dir, "eicu_formal_v1_source_apacheapsvar.csv")
out_pred <- file.path(out_dir, "eicu_formal_v1_source_apachepredvar.csv")
out_summary <- file.path(out_dir, "eicu_formal_v1_source_summary.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_source_report.txt")

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

sql_literal <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
sql_in <- function(x) paste(sql_literal(x), collapse = ", ")
values_ids <- function(ids) paste0("(", paste(as.integer(ids), collapse = "),("), ")")

run_query <- function(sql, label) {
  sql <- sub(";\\s*$", "", sql)
  sql_file <- tempfile(fileext = ".sql")
  writeLines(paste0("copy (", sql, ") to stdout with csv header;"), sql_file, useBytes = TRUE)
  on.exit(unlink(sql_file), add = TRUE)
  t0 <- Sys.time()
  res <- system2(
    psql_exe,
    args = c(
      "--no-psqlrc", "-h", pg_host, "-p", pg_port, "-U", pg_user,
      "-d", pg_db, "-v", "ON_ERROR_STOP=1", "-P", "pager=off", "-q", "-f", sql_file
    ),
    stdout = TRUE,
    stderr = TRUE,
    timeout = query_timeout_sec
  )
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    cat(paste(res, collapse = "\n"), "\n")
    stop(label, " failed or exceeded ", query_timeout_sec, " seconds.", call. = FALSE)
  }
  dt <- fread(text = paste(res, collapse = "\n"), showProgress = FALSE)
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(label, ": rows=", nrow(dt), "; seconds=", elapsed, "\n", sep = "")
  dt
}

run_chunked <- function(ids, sql_builder, label) {
  print_section(label)
  ids <- unique(as.integer(ids))
  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  out <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    out[[i]] <- run_query(sql_builder(chunks[[i]]), paste0(label, " chunk ", i, "/", length(chunks)))
  }
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

audit_tables_dir <- file.path(base_dir, "audit_from_validation", "tables")
sepsis_terms_file <- file.path(audit_tables_dir, "eicu_v02_sepsis_label_review.csv")
abx_terms_file <- file.path(audit_tables_dir, "eicu_v02_antibiotic_term_review.csv")
broad_cache_file <- file.path(audit_tables_dir, "eicu_v01_validation_cohort_abx_audit.csv")

if (!file.exists(sepsis_terms_file)) stop("Missing reviewed sepsis term file: ", sepsis_terms_file, call. = FALSE)
if (!file.exists(abx_terms_file)) stop("Missing reviewed antibiotic term file: ", abx_terms_file, call. = FALSE)
if (!file.exists(broad_cache_file)) stop("Missing broad-sepsis prefilter cache: ", broad_cache_file, call. = FALSE)

sepsis_terms <- fread(sepsis_terms_file, showProgress = FALSE)
abx_terms <- fread(abx_terms_file, showProgress = FALSE)
broad_cache <- fread(broad_cache_file, select = "patientunitstayid", showProgress = FALSE)
prefilter_ids <- unique(broad_cache$patientunitstayid)

explicit_sepsis_terms <- sepsis_terms[suggested_sepsis_role == "primary_explicit_sepsis"]
diagnosis_terms <- explicit_sepsis_terms[label_source == "diagnosis", unique(label_text)]
admissiondx_terms <- explicit_sepsis_terms[label_source == "admissiondx", unique(label_text)]

primary_abx_terms <- abx_terms[flag_po_or_local %in% c(FALSE, "f", "FALSE", "False", 0, "0")]
medication_terms <- primary_abx_terms[abx_source == "medication", unique(drugname)]
infusion_terms <- primary_abx_terms[abx_source == "infusiondrug", unique(drugname)]

if (length(diagnosis_terms) == 0) stop("No reviewed explicit diagnosis sepsis terms.", call. = FALSE)
if (length(admissiondx_terms) == 0) stop("No reviewed explicit admissiondx sepsis terms.", call. = FALSE)
if (length(medication_terms) == 0) stop("No reviewed medication antibiotic terms.", call. = FALSE)
if (length(infusion_terms) == 0) stop("No reviewed infusion antibiotic terms.", call. = FALSE)

diagnosis_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select d.patientunitstayid,
       d.diagnosisoffset as label_offset_min,
       'diagnosis'::text as label_source,
       d.diagnosisstring::text as label_text
from eicu_crd.diagnosis d
join target t using (patientunitstayid)
where d.diagnosisstring in (%s)
", values_ids(ids), sql_in(diagnosis_terms))

admissiondx_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select a.patientunitstayid,
       a.admitdxenteredoffset as label_offset_min,
       'admissiondx'::text as label_source,
       concat_ws(' | ', a.admitdxpath, a.admitdxname, a.admitdxtext)::text as label_text
from eicu_crd.admissiondx a
join target t using (patientunitstayid)
where concat_ws(' | ', a.admitdxpath, a.admitdxname, a.admitdxtext) in (%s)
", values_ids(ids), sql_in(admissiondx_terms))

patient_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select p.patientunitstayid,
       p.patienthealthsystemstayid,
       p.uniquepid,
       p.hospitalid,
       p.wardid,
       p.gender as gender_raw,
       p.age as age_raw,
       case when p.age ~ '^[0-9]+$' then p.age::int when p.age = '> 89' then 90 else null end as age_num_raw,
       p.ethnicity as ethnicity_raw,
       p.hospitaladmitsource as hospitaladmitsource_raw,
       p.unitadmitsource as unitadmitsource_raw,
       p.unittype as unittype_raw,
       p.unitstaytype as unitstaytype_raw,
       p.unitvisitnumber,
       p.apacheadmissiondx as apacheadmissiondx_raw,
       p.hospitaladmitoffset,
       p.admissionheight as admissionheight_raw,
       p.admissionweight as admissionweight_raw,
       p.hospitaldischargeoffset,
       p.hospitaldischargestatus,
       p.hospitaldischargelocation,
       p.unitdischargeoffset,
       p.unitdischargestatus,
       p.unitdischargelocation
from eicu_crd.patient p
join target t using (patientunitstayid)
", values_ids(ids))

med_abx_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select m.patientunitstayid,
       m.drugstartoffset as abx_offset_min,
       m.drugname::text as drugname,
       'medication'::text as abx_source
from eicu_crd.medication m
join target t using (patientunitstayid)
where m.drugname in (%s)
  and coalesce(m.drugordercancelled, '') !~* 'yes|true|cancel'
  and m.drugstartoffset is not null
", values_ids(ids), sql_in(medication_terms))

inf_abx_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select i.patientunitstayid,
       i.infusionoffset as abx_offset_min,
       i.drugname::text as drugname,
       'infusiondrug'::text as abx_source
from eicu_crd.infusiondrug i
join target t using (patientunitstayid)
where i.drugname in (%s)
  and i.infusionoffset is not null
", values_ids(ids), sql_in(infusion_terms))

apr_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select a.patientunitstayid,
       a.acutephysiologyscore as aps_iva_raw,
       a.apachescore as apache_score_iva_raw,
       a.predictedhospitalmortality::text as pred_hosp_mort_iva_raw,
       a.predictedicumortality::text as pred_icu_mort_iva_raw
from eicu_crd.apachepatientresult a
join target t using (patientunitstayid)
where a.apacheversion = 'IVa'
", values_ids(ids))

aps_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select a.patientunitstayid,
       a.intubated as intubated_apache_raw,
       a.vent as vent_apache_raw,
       a.dialysis as dialysis_apache_raw,
       a.eyes as eyes_apache_raw,
       a.motor as motor_apache_raw,
       a.verbal as verbal_apache_raw,
       a.meds as meds_apache_raw,
       a.urine as urine_apache_raw,
       a.wbc as wbc_apache_raw,
       a.temperature as temperature_apache_raw,
       a.respiratoryrate as respiratoryrate_apache_raw,
       a.sodium as sodium_apache_raw,
       a.heartrate as heartrate_apache_raw,
       a.meanbp as meanbp_apache_raw,
       a.ph as ph_apache_raw,
       a.hematocrit as hematocrit_apache_raw,
       a.creatinine as creatinine_apache_raw,
       a.albumin as albumin_apache_raw,
       a.pao2 as pao2_apache_raw,
       a.pco2 as pco2_apache_raw,
       a.bun as bun_apache_raw,
       a.glucose as glucose_apache_raw,
       a.bilirubin as bilirubin_apache_raw,
       a.fio2 as fio2_apache_raw
from eicu_crd.apacheapsvar a
join target t using (patientunitstayid)
", values_ids(ids))

pred_builder <- function(ids) sprintf("
with target(patientunitstayid) as (values %s)
select p.patientunitstayid,
       p.aids as aids_pred_raw,
       p.hepaticfailure as hepaticfailure_pred_raw,
       p.lymphoma as lymphoma_pred_raw,
       p.metastaticcancer as metastaticcancer_pred_raw,
       p.leukemia as leukemia_pred_raw,
       p.immunosuppression as immunosuppression_pred_raw,
       p.cirrhosis as cirrhosis_pred_raw,
       p.diabetes as diabetes_pred_raw,
       p.electivesurgery as electivesurgery_pred_raw,
       p.readmit as readmit_pred_raw,
       p.ventday1 as ventday1_pred_raw,
       p.oobventday1 as oobventday1_pred_raw,
       p.oobintubday1 as oobintubday1_pred_raw,
       p.day1meds as day1meds_pred_raw,
       p.day1verbal as day1verbal_pred_raw,
       p.day1motor as day1motor_pred_raw,
       p.day1eyes as day1eyes_pred_raw,
       p.day1pao2 as day1pao2_pred_raw,
       p.day1fio2 as day1fio2_pred_raw
from eicu_crd.apachepredvar p
join target t using (patientunitstayid)
", values_ids(ids))

diagnosis_sepsis <- run_chunked(prefilter_ids, diagnosis_builder, "01a diagnosis sepsis label rows from broad-cache IDs")
admissiondx_sepsis <- run_chunked(prefilter_ids, admissiondx_builder, "01b admissiondx sepsis label rows from broad-cache IDs")
sepsis_rows <- rbindlist(list(diagnosis_sepsis, admissiondx_sepsis), use.names = TRUE)

strict_sepsis <- sepsis_rows[
  ,
  .(
    sepsis_label_first_offset_min = min(label_offset_min, na.rm = TRUE),
    sepsis_label_rows = .N,
    has_diagnosis_sepsis = any(label_source == "diagnosis"),
    has_admissiondx_sepsis = any(label_source == "admissiondx")
  ),
  by = patientunitstayid
]

patient <- run_chunked(strict_sepsis$patientunitstayid, patient_builder, "01c patient rows for strict sepsis IDs")
med_abx <- run_chunked(strict_sepsis$patientunitstayid, med_abx_builder, "01d medication antibiotic rows for strict sepsis IDs")
inf_abx <- run_chunked(strict_sepsis$patientunitstayid, inf_abx_builder, "01e infusion antibiotic rows for strict sepsis IDs")
apr <- run_chunked(strict_sepsis$patientunitstayid, apr_builder, "01f APACHE IVa rows for strict sepsis IDs")
aps <- run_chunked(strict_sepsis$patientunitstayid, aps_builder, "01g APACHE APS rows for strict sepsis IDs")
pred <- run_chunked(strict_sepsis$patientunitstayid, pred_builder, "01h APACHE pred rows for strict sepsis IDs")

abx_rows <- rbindlist(list(med_abx, inf_abx), use.names = TRUE)
setorder(abx_rows, patientunitstayid, abx_offset_min, abx_source, drugname)
first_abx <- abx_rows[, .SD[1], by = patientunitstayid]
setnames(first_abx, c("abx_offset_min", "drugname", "abx_source"), c("first_abx_offset_min", "first_abx_drugname", "first_abx_source"))

cohort_all <- merge(strict_sepsis, patient, by = "patientunitstayid", all = FALSE, sort = FALSE)
cohort_all <- merge(cohort_all, first_abx, by = "patientunitstayid", all.x = TRUE, sort = FALSE)

cohort_all[, first_abx_delay_hours_from_unit := round(first_abx_offset_min / 60, 3)]
cohort_all[, abx_window_unit_t0 := fifelse(
  is.na(first_abx_offset_min), "no_abx_record",
  fifelse(first_abx_offset_min < 0, "pre_t0_abx",
  fifelse(first_abx_offset_min <= 180, "0-3h",
  fifelse(first_abx_offset_min <= 360, "3-6h",
  fifelse(first_abx_offset_min <= 720, "6-12h",
  fifelse(first_abx_offset_min <= 4320, ">12-72h", ">72h")))))
)]
cohort_all[, death_30d_inhosp := fifelse(
  tolower(fifelse(is.na(hospitaldischargestatus), "", hospitaldischargestatus)) == "expired" &
    !is.na(hospitaldischargeoffset) & hospitaldischargeoffset <= 43200,
  1L,
  fifelse(is.na(hospitaldischargestatus) | hospitaldischargestatus == "", NA_integer_, 0L)
)]
cohort_all[, death_hosp := fifelse(
  tolower(fifelse(is.na(hospitaldischargestatus), "", hospitaldischargestatus)) == "expired",
  1L,
  fifelse(is.na(hospitaldischargestatus) | hospitaldischargestatus == "", NA_integer_, 0L)
)]
cohort_all[, death_icu := fifelse(
  tolower(fifelse(is.na(unitdischargestatus), "", unitdischargestatus)) == "expired",
  1L,
  fifelse(is.na(unitdischargestatus) | unitdischargestatus == "", NA_integer_, 0L)
)]
cohort_all[, patient_key_for_first := fifelse(is.na(uniquepid) | uniquepid == "", as.character(patientunitstayid), as.character(uniquepid))]
setorder(cohort_all, patient_key_for_first, patientunitstayid)
cohort_all[, rn_patient := seq_len(.N), by = patient_key_for_first]

cohort <- cohort_all[rn_patient == 1]
cohort[, `:=`(
  t0_offset_min = 0L,
  t0_definition = "ICU admission offset 0",
  pre_t0_abx = as.integer(abx_window_unit_t0 == "pre_t0_abx"),
  no_abx_record = as.integer(abx_window_unit_t0 == "no_abx_record"),
  main_three_window_candidate = as.integer(abx_window_unit_t0 %in% c("0-3h", "3-6h", "6-12h")),
  post_t0_abx_0_72h = as.integer(abx_window_unit_t0 %in% c("0-3h", "3-6h", "6-12h", ">12-72h")),
  followup_30d_inhosp_days = fifelse(!is.na(hospitaldischargeoffset) & hospitaldischargeoffset >= 0, pmin(hospitaldischargeoffset, 43200) / 1440, NA_real_)
)]
cohort[, patient_key_for_first := NULL]
setorder(cohort, patientunitstayid)

expected_windows <- c(
  pre_t0_abx = 7571L,
  `0-3h` = 2443L,
  `3-6h` = 820L,
  `6-12h` = 622L,
  `>12-72h` = 1393L,
  `>72h` = 453L,
  no_abx_record = 11610L
)
window_counts <- table(cohort$abx_window_unit_t0)
for (nm in names(expected_windows)) {
  if (!identical(as.integer(window_counts[[nm]]), expected_windows[[nm]])) {
    stop("Hard check failed for window ", nm, ": got ", as.integer(window_counts[[nm]]), " expected ", expected_windows[[nm]], call. = FALSE)
  }
}
if (nrow(cohort) != 24912L) stop("Hard check failed: cohort N is not 24912.", call. = FALSE)
if (uniqueN(cohort$patientunitstayid) != 24912L) stop("Hard check failed: patientunitstayid is not unique.", call. = FALSE)
if (sum(cohort$death_30d_inhosp == 1, na.rm = TRUE) != 4619L) stop("Hard check failed: death_30d_inhosp count is not 4619.", call. = FALSE)

fwrite(sepsis_rows, out_sepsis_labels)
fwrite(strict_sepsis, out_strict_sepsis)
fwrite(abx_rows, out_abx_rows)
fwrite(first_abx, out_first_abx)
fwrite(cohort, out_cohort)
fwrite(apr, out_apr)
fwrite(aps, out_aps)
fwrite(pred, out_pred)

summary_tbl <- rbindlist(list(
  data.table(source = "reviewed_terms", metric = "diagnosis_sepsis_terms", value = length(diagnosis_terms)),
  data.table(source = "reviewed_terms", metric = "admissiondx_sepsis_terms", value = length(admissiondx_terms)),
  data.table(source = "reviewed_terms", metric = "medication_abx_terms", value = length(medication_terms)),
  data.table(source = "reviewed_terms", metric = "infusion_abx_terms", value = length(infusion_terms)),
  data.table(source = "broad_cache_prefilter", metric = "candidate_stays", value = length(prefilter_ids)),
  data.table(source = "sepsis_label_rows", metric = "rows", value = nrow(sepsis_rows)),
  data.table(source = "strict_sepsis_stays", metric = "rows", value = nrow(strict_sepsis)),
  data.table(source = "antibiotic_candidate_rows", metric = "rows", value = nrow(abx_rows)),
  data.table(source = "first_antibiotic", metric = "rows", value = nrow(first_abx)),
  data.table(source = "cohort_exposure_outcome", metric = "rows", value = nrow(cohort)),
  data.table(source = "cohort_exposure_outcome", metric = "distinct_patientunitstayid", value = uniqueN(cohort$patientunitstayid)),
  data.table(source = "cohort_exposure_outcome", metric = "death_30d_inhosp_n", value = sum(cohort$death_30d_inhosp == 1, na.rm = TRUE)),
  data.table(source = "apache_iva", metric = "rows_in_cohort_ids", value = nrow(apr)),
  data.table(source = "apacheapsvar", metric = "rows_in_cohort_ids", value = nrow(aps)),
  data.table(source = "apachepredvar", metric = "rows_in_cohort_ids", value = nrow(pred))
), use.names = TRUE)
summary_tbl <- rbind(
  summary_tbl,
  data.table(source = "cohort_exposure_outcome", metric = paste0("window_", names(expected_windows), "_n"), value = as.integer(window_counts[names(expected_windows)])),
  use.names = TRUE
)
fwrite(summary_tbl, out_summary)

report_lines <- c(
  "eICU formal_v1 step 01 source extraction report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Database: ", pg_db, " / schema eicu_crd"),
  paste0("Per-query timeout seconds: ", query_timeout_sec),
  paste0("Chunk size: ", chunk_size),
  paste0("Reviewed sepsis term file: ", sepsis_terms_file),
  paste0("Reviewed antibiotic term file: ", abx_terms_file),
  paste0("Broad-sepsis prefilter cache: ", broad_cache_file),
  "",
  "Definition:",
  "- strict explicit sepsis label from reviewed exact label terms",
  "- broad adult sepsis audit cache is used only as an ID prefilter; hard checks enforce the locked strict cohort counts",
  "- first qualifying stay per uniquepid",
  "- t0 = ICU admission offset 0",
  "- antibiotics from reviewed exact drugname terms excluding PO/local route records",
  "- outcome = 30-day in-hospital mortality",
  "",
  "Outputs:",
  paste0("- ", out_sepsis_labels),
  paste0("- ", out_strict_sepsis),
  paste0("- ", out_abx_rows),
  paste0("- ", out_first_abx),
  paste0("- ", out_cohort),
  paste0("- ", out_apr),
  paste0("- ", out_aps),
  paste0("- ", out_pred),
  paste0("- ", out_summary),
  "",
  "Summary:",
  paste(capture.output(print(summary_tbl)), collapse = "\n")
)
writeLines(report_lines, out_report, useBytes = TRUE)

cat("Wrote:\n")
cat(out_sepsis_labels, "\n")
cat(out_strict_sepsis, "\n")
cat(out_abx_rows, "\n")
cat(out_first_abx, "\n")
cat(out_cohort, "\n")
cat(out_apr, "\n")
cat(out_aps, "\n")
cat(out_pred, "\n")
cat(out_summary, "\n")
cat(out_report, "\n")
