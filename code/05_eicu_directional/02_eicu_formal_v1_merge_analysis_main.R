# eICU formal_v1 step 02: merge analysis_main
# MIMIC counterpart: 02_abx_tte_merge_analysis_main_formal_v1_20260612.sql
# Stage: wide table merge

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

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

in_cohort <- file.path(out_dir, "eicu_formal_v1_source_cohort_exposure_outcome.csv")
in_apr <- file.path(out_dir, "eicu_formal_v1_source_apache_iva.csv")
in_aps <- file.path(out_dir, "eicu_formal_v1_source_apacheapsvar.csv")
in_pred <- file.path(out_dir, "eicu_formal_v1_source_apachepredvar.csv")
out_main <- file.path(out_dir, "eicu_formal_v1_analysis_main.csv")
out_summary <- file.path(out_dir, "eicu_formal_v1_analysis_main_summary.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_analysis_main_report.txt")

missing_inputs <- c(in_cohort, in_apr, in_aps, in_pred)[!file.exists(c(in_cohort, in_apr, in_aps, in_pred))]
if (length(missing_inputs) > 0) {
  stop("Missing step 01 input(s): ", paste(missing_inputs, collapse = "; "), call. = FALSE)
}

cohort <- fread(in_cohort, showProgress = FALSE)
apr <- fread(in_apr, showProgress = FALSE)
aps <- fread(in_aps, showProgress = FALSE)
pred <- fread(in_pred, showProgress = FALSE)

ids <- cohort$patientunitstayid
apr <- apr[patientunitstayid %in% ids]
aps <- aps[patientunitstayid %in% ids]
pred <- pred[patientunitstayid %in% ids]

if (anyDuplicated(cohort$patientunitstayid)) stop("Duplicate patientunitstayid in cohort source.", call. = FALSE)
if (anyDuplicated(apr$patientunitstayid)) stop("Duplicate patientunitstayid in APACHE IVa source.", call. = FALSE)
if (anyDuplicated(aps$patientunitstayid)) stop("Duplicate patientunitstayid in apacheapsvar source.", call. = FALSE)
if (anyDuplicated(pred$patientunitstayid)) stop("Duplicate patientunitstayid in apachepredvar source.", call. = FALSE)

main <- merge(cohort, apr, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
main <- merge(main, aps, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
main <- merge(main, pred, by = "patientunitstayid", all.x = TRUE, sort = FALSE)
setorder(main, patientunitstayid)

expected_windows <- c(
  pre_t0_abx = 7571L,
  `0-3h` = 2443L,
  `3-6h` = 820L,
  `6-12h` = 622L,
  `>12-72h` = 1393L,
  `>72h` = 453L,
  no_abx_record = 11610L
)
window_counts <- table(main$abx_window_unit_t0)
for (nm in names(expected_windows)) {
  if (!identical(as.integer(window_counts[[nm]]), expected_windows[[nm]])) {
    stop("Hard check failed for window ", nm, call. = FALSE)
  }
}
if (nrow(main) != 24912L) stop("Hard check failed: analysis_main N is not 24912.", call. = FALSE)
if (uniqueN(main$patientunitstayid) != 24912L) stop("Hard check failed: patientunitstayid is not unique.", call. = FALSE)
if (sum(main$death_30d_inhosp == 1, na.rm = TRUE) != 4619L) stop("Hard check failed: death_30d_inhosp count is not 4619.", call. = FALSE)

main[, apacheapsvar_available := as.integer(!is.na(intubated_apache_raw))]
main[, apachepredvar_available := as.integer(!is.na(aids_pred_raw))]
main[, apachepatientresult_iva_available := as.integer(!is.na(aps_iva_raw))]

summary_tbl <- data.table(
  metric = c(
    "cohort_n",
    "distinct_patientunitstayid_n",
    "duplicate_patientunitstayid_rows_n",
    "unique_pid_n",
    "hospital_n",
    "death_30d_inhosp_n",
    "death_30d_inhosp_pct",
    "pre_t0_abx_n",
    "no_abx_record_n",
    "main_three_window_candidate_n",
    "post_t0_abx_0_72h_n",
    "window_0_3h_n",
    "window_3_6h_n",
    "window_6_12h_n",
    "window_gt12_72h_n",
    "window_gt72h_n",
    "apacheapsvar_available_n",
    "apachepredvar_available_n",
    "apachepatientresult_iva_available_n"
  ),
  value = as.character(c(
    nrow(main),
    uniqueN(main$patientunitstayid),
    nrow(main) - uniqueN(main$patientunitstayid),
    uniqueN(main$uniquepid),
    uniqueN(main$hospitalid),
    sum(main$death_30d_inhosp == 1, na.rm = TRUE),
    round(100 * mean(main$death_30d_inhosp == 1, na.rm = TRUE), 2),
    sum(main$abx_window_unit_t0 == "pre_t0_abx", na.rm = TRUE),
    sum(main$abx_window_unit_t0 == "no_abx_record", na.rm = TRUE),
    sum(main$main_three_window_candidate == 1, na.rm = TRUE),
    sum(main$post_t0_abx_0_72h == 1, na.rm = TRUE),
    sum(main$abx_window_unit_t0 == "0-3h", na.rm = TRUE),
    sum(main$abx_window_unit_t0 == "3-6h", na.rm = TRUE),
    sum(main$abx_window_unit_t0 == "6-12h", na.rm = TRUE),
    sum(main$abx_window_unit_t0 == ">12-72h", na.rm = TRUE),
    sum(main$abx_window_unit_t0 == ">72h", na.rm = TRUE),
    sum(main$apacheapsvar_available == 1, na.rm = TRUE),
    sum(main$apachepredvar_available == 1, na.rm = TRUE),
    sum(main$apachepatientresult_iva_available == 1, na.rm = TRUE)
  ))
)

fwrite(main, out_main)
fwrite(summary_tbl, out_summary)

report_lines <- c(
  "eICU formal_v1 step 02 analysis_main merge report",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "Inputs:",
  paste0("- ", in_cohort),
  paste0("- ", in_apr),
  paste0("- ", in_aps),
  paste0("- ", in_pred),
  "",
  "Outputs:",
  paste0("- ", out_main),
  paste0("- ", out_summary),
  "",
  "Summary:",
  paste(capture.output(print(summary_tbl)), collapse = "\n")
)
writeLines(report_lines, out_report, useBytes = TRUE)

cat("Wrote:\n")
cat(out_main, "\n")
cat(out_summary, "\n")
cat(out_report, "\n")
