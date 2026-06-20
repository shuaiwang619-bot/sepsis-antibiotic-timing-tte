# eICU formal_v1 step 06: arm2 hourly input before CCW/IPCW
# MIMIC counterpart: 06_formal_v1_long_table_pre_ccw_qc.R
#
# Scope:
#   Build a compact MICE-aware hourly table for the exploratory eICU arm2
#   CCW/IPCW supplement. eICU has no reliable MIMIC-like time-varying SOFA
#   stream in this validation flow, so this table carries baseline covariates
#   across hours 0-12 and is used only for artificial-censoring weights.
#
# Input:
#   outputs/mice/eicu_formal_v1_arm2_model_dataset_mice_m5_model_ready.csv
#   outputs/wide_tables/eicu_formal_v1_analysis_main_model_ready_wide.csv
#
# Outputs:
#   outputs/ccw/eicu_formal_v1_ccw_arm2_hourly_input_m5.csv
#   outputs/ccw/eicu_formal_v1_ccw_arm2_hourly_input_diagnostics.csv
#   outputs/ccw/eicu_formal_v1_ccw_arm2_hourly_input_report.txt

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
mice_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "mice", "eicu_formal_v1_arm2_model_dataset_mice_m5_model_ready.csv")
}
wide_file <- if (length(args) >= 3) {
  args[3]
} else {
  file.path(base_dir, "outputs", "wide_tables", "eicu_formal_v1_analysis_main_model_ready_wide.csv")
}

out_dir <- file.path(base_dir, "outputs", "ccw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_hourly <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_hourly_input_m5.csv")
out_diag <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_hourly_input_diagnostics.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_hourly_input_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

as_int01 <- function(x) {
  z <- suppressWarnings(as.integer(as.character(x)))
  z[!(z %in% c(0L, 1L))] <- NA_integer_
  z
}

window_from_delay <- function(delay) {
  out <- rep(NA_character_, length(delay))
  out[!is.na(delay) & delay >= 0 & delay <= 3] <- "0-3h"
  out[!is.na(delay) & delay > 3 & delay <= 12] <- "3-12h"
  out
}

stop_if_missing(mice_file, "Arm2 MICE model-ready file")
stop_if_missing(wide_file, "Full model-ready wide table")

need_mice <- c(
  ".imp", "patientunitstayid", "uniquepid", "hospitalid",
  "death_30d_inhosp", "arm2_abx_timing", "arm2_late_3_12h",
  "first_abx_offset_min", "first_abx_delay_hours_from_unit",
  "gender_factor", "ethnicity_group", "unit_admit_source_group",
  "unit_type_group", "hosp_window_volume_bucket_factor",
  "hosp_zero_abx_reporting_ind",
  "age_model", "apache_score_iva_model",
  "apache_score_iva_model_missing_ind",
  "diabetes_model", "diabetes_model_missing_ind",
  "vent_model", "vent_model_missing_ind",
  "intubated_model", "intubated_model_missing_ind",
  "dialysis_model", "dialysis_model_missing_ind",
  "cirrhosis_model", "cirrhosis_model_missing_ind",
  "immunosuppression_model", "immunosuppression_model_missing_ind",
  "hepaticfailure_model", "hepaticfailure_model_missing_ind",
  "metastaticcancer_model", "metastaticcancer_model_missing_ind"
)
need_wide <- c("patientunitstayid", "followup_30d_inhosp_days", "abx_window_unit_t0")

mice_header <- names(fread(mice_file, nrows = 0, showProgress = FALSE))
wide_header <- names(fread(wide_file, nrows = 0, showProgress = FALSE))
missing_mice <- setdiff(need_mice, mice_header)
missing_wide <- setdiff(need_wide, wide_header)
if (length(missing_mice) > 0) stop("MICE file missing columns: ", paste(missing_mice, collapse = ", "), call. = FALSE)
if (length(missing_wide) > 0) stop("Wide file missing columns: ", paste(missing_wide, collapse = ", "), call. = FALSE)

d <- fread(mice_file, select = need_mice, showProgress = FALSE)
w <- fread(wide_file, select = need_wide, showProgress = FALSE)

d[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  hospitalid = as.integer(to_num(hospitalid)),
  death_30d_inhosp = as_int01(death_30d_inhosp),
  arm2_late_3_12h = as_int01(arm2_late_3_12h),
  first_abx_offset_min = to_num(first_abx_offset_min),
  first_abx_delay_hours_from_unit = to_num(first_abx_delay_hours_from_unit),
  hosp_zero_abx_reporting_ind = as_int01(hosp_zero_abx_reporting_ind),
  age_model = to_num(age_model),
  apache_score_iva_model = to_num(apache_score_iva_model),
  apache_score_iva_model_missing_ind = as_int01(apache_score_iva_model_missing_ind),
  diabetes_model = as_int01(diabetes_model),
  diabetes_model_missing_ind = as_int01(diabetes_model_missing_ind),
  vent_model = as_int01(vent_model),
  vent_model_missing_ind = as_int01(vent_model_missing_ind),
  intubated_model = as_int01(intubated_model),
  intubated_model_missing_ind = as_int01(intubated_model_missing_ind),
  dialysis_model = as_int01(dialysis_model),
  dialysis_model_missing_ind = as_int01(dialysis_model_missing_ind),
  cirrhosis_model = as_int01(cirrhosis_model),
  cirrhosis_model_missing_ind = as_int01(cirrhosis_model_missing_ind),
  immunosuppression_model = as_int01(immunosuppression_model),
  immunosuppression_model_missing_ind = as_int01(immunosuppression_model_missing_ind),
  hepaticfailure_model = as_int01(hepaticfailure_model),
  hepaticfailure_model_missing_ind = as_int01(hepaticfailure_model_missing_ind),
  metastaticcancer_model = as_int01(metastaticcancer_model),
  metastaticcancer_model_missing_ind = as_int01(metastaticcancer_model_missing_ind)
)]
w[, `:=`(
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  followup_30d_inhosp_days = to_num(followup_30d_inhosp_days)
)]

d <- d[.imp %in% 1:5]
d <- d[!is.na(death_30d_inhosp) & arm2_abx_timing %in% c("0-3h", "3-12h")]

if (d[, anyDuplicated(paste(.imp, patientunitstayid))] > 0) {
  stop("Duplicate .imp + patientunitstayid rows in arm2 MICE input.", call. = FALSE)
}
if (w[, anyDuplicated(patientunitstayid)] > 0) {
  stop("Duplicate patientunitstayid rows in full wide table.", call. = FALSE)
}

d <- merge(d, w, by = "patientunitstayid", all.x = TRUE, sort = FALSE)

d[, recalculated_arm2_window := window_from_delay(first_abx_delay_hours_from_unit)]
boundary_diag <- d[, .(
  rows = .N,
  distinct_stays = uniqueN(patientunitstayid),
  mismatch_n = sum(arm2_abx_timing != recalculated_arm2_window, na.rm = TRUE),
  missing_delay_n = sum(is.na(first_abx_delay_hours_from_unit)),
  missing_followup_n = sum(is.na(followup_30d_inhosp_days))
), by = .imp][order(.imp)]

if (boundary_diag[, any(mismatch_n > 0 | missing_delay_n > 0 | missing_followup_n > 0)]) {
  print(boundary_diag)
  stop("Arm2 timing/follow-up boundary check failed.", call. = FALSE)
}

required_model_cols <- c(
  "age_model", "apache_score_iva_model", "apache_score_iva_model_missing_ind",
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "diabetes_model_missing_ind"
)
model_na <- d[, lapply(.SD, function(x) sum(is.na(x))), by = .imp, .SDcols = required_model_cols]
if (model_na[, any(rowSums(.SD) > 0), .SDcols = required_model_cols]) {
  print(model_na)
  stop("Main CCW/IPCW baseline model columns contain NA after MICE.", call. = FALSE)
}

d[, surv_hours_30d := pmin(30 * 24, pmax(0, followup_30d_inhosp_days * 24))]
d[, observed_last_hour_for_ipcw := pmin(12L, floor(surv_hours_30d))]

hour_grid <- data.table(t_hour = 0:12)
d[, tmp_cross_join := 1L]
hour_grid[, tmp_cross_join := 1L]
long <- merge(d, hour_grid, by = "tmp_cross_join", allow.cartesian = TRUE, sort = FALSE)
long[, tmp_cross_join := NULL]
d[, tmp_cross_join := NULL]
hour_grid[, tmp_cross_join := NULL]

long[, still_observed_i := as.integer(t_hour <= observed_last_hour_for_ipcw)]
setorder(long, .imp, patientunitstayid, t_hour)

diag_overall <- long[, .(
  rows = .N,
  distinct_stays = uniqueN(patientunitstayid),
  min_hour = min(t_hour),
  max_hour = max(t_hour),
  still_observed_rows = sum(still_observed_i),
  death_n = uniqueN(patientunitstayid[death_30d_inhosp == 1L])
), by = .imp][order(.imp)]
diag_by_arm <- unique(long[, .(
  .imp, patientunitstayid, arm2_abx_timing, death_30d_inhosp
)])[, .(
  stays = .N,
  deaths = sum(death_30d_inhosp),
  death_pct = round(100 * mean(death_30d_inhosp), 2)
), by = .(.imp, arm2_abx_timing)][order(.imp, arm2_abx_timing)]

diag_out <- rbindlist(list(
  cbind(section = "boundary", boundary_diag),
  cbind(section = "overall_hourly", diag_overall),
  cbind(section = "by_arm", diag_by_arm),
  cbind(section = "model_na", model_na)
), fill = TRUE)

fwrite(long, out_hourly)
fwrite(diag_out, out_diag)

report_lines <- c(
  "eICU formal_v1 step 06 arm2 hourly input before CCW/IPCW",
  paste("mice_file:", mice_file),
  paste("wide_file:", wide_file),
  "",
  "boundary diagnostics:",
  capture.output(print(boundary_diag)),
  "",
  "hourly shape:",
  capture.output(print(diag_overall)),
  "",
  "arm2 exposure/outcome:",
  capture.output(print(diag_by_arm)),
  "",
  "notes:",
  "This is a baseline-carried hourly table for ICU-admission anchored eICU arm2.",
  "No eICU time-varying SOFA stream is used in this exploratory supplement.",
  "Rows cover t_hour 0-12 for artificial-censoring IPCW only."
)
writeLines(report_lines, out_report)

message("Wrote hourly input: ", out_hourly)
message("Wrote diagnostics: ", out_diag)
message("Wrote report: ", out_report)
