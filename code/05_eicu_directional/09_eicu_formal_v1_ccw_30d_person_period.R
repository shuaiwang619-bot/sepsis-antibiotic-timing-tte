# eICU formal_v1 step 09: arm2 CCW/IPCW 30-day person-period table
# MIMIC counterpart: 09_formal_v1_ccw_30d_person_period.R
#
# Time grid:
#   h0-h23      = hourly periods over the first 24 hours
#   day2-day30  = daily periods from 24 hours through 30 days

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
weights_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_ipcw_final_weights_m5.csv")
}
time_weights_file <- if (length(args) >= 3) {
  args[3]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_ipcw_time_weights_m5.csv")
}

out_dir <- file.path(base_dir, "outputs", "ccw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_pp <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_30d_person_period_m5.csv")
out_diag <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_30d_person_period_diagnostics.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_30d_person_period_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

stop_if_missing(weights_file, "Step 08 final weights")
stop_if_missing(time_weights_file, "Step 08 time weights")

weights <- fread(weights_file, showProgress = FALSE)
time_weights <- fread(time_weights_file, showProgress = FALSE)

need_weights <- c(
  ".imp", "clone_id", "patientunitstayid", "uniquepid", "hospitalid",
  "arm", "arm_id", "arm_start_h", "arm_end_h",
  "death_30d_inhosp", "followup_30d_inhosp_days", "surv_hours_30d",
  "artificial_censor_h", "artificial_censor_reason",
  "strategy_adherent_by_delay", "natural_censor_before_arm_end", "empty_ipcw_product",
  "final_ipcw", "final_ipcw_trunc_p01_p99", "final_ipcw_trunc_p05_p95",
  "age_model", "apache_score_iva_model", "apache_score_iva_model_missing_ind",
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "diabetes_model_missing_ind",
  "vent_model", "intubated_model", "dialysis_model",
  "cirrhosis_model", "immunosuppression_model", "hepaticfailure_model", "metastaticcancer_model"
)
need_time <- c(".imp", "clone_id", "patientunitstayid", "arm", "arm_id", "t_hour", "ipcw_t")
missing_weights <- setdiff(need_weights, names(weights))
missing_time <- setdiff(need_time, names(time_weights))
if (length(missing_weights) > 0) stop("Weights file missing columns: ", paste(missing_weights, collapse = ", "), call. = FALSE)
if (length(missing_time) > 0) stop("Time weights file missing columns: ", paste(missing_time, collapse = ", "), call. = FALSE)

weights[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  hospitalid = as.integer(to_num(hospitalid)),
  arm_id = as.integer(to_num(arm_id)),
  arm_start_h = as.integer(to_num(arm_start_h)),
  arm_end_h = as.integer(to_num(arm_end_h)),
  death_30d_inhosp = as.integer(to_num(death_30d_inhosp)),
  followup_30d_inhosp_days = to_num(followup_30d_inhosp_days),
  surv_hours_30d = to_num(surv_hours_30d),
  artificial_censor_h = to_num(artificial_censor_h),
  strategy_adherent_by_delay = as.logical(strategy_adherent_by_delay),
  natural_censor_before_arm_end = as.integer(to_num(natural_censor_before_arm_end)),
  empty_ipcw_product = as.integer(to_num(empty_ipcw_product)),
  final_ipcw = to_num(final_ipcw),
  final_ipcw_trunc_p01_p99 = to_num(final_ipcw_trunc_p01_p99),
  final_ipcw_trunc_p05_p95 = to_num(final_ipcw_trunc_p05_p95),
  age_model = to_num(age_model),
  apache_score_iva_model = to_num(apache_score_iva_model),
  apache_score_iva_model_missing_ind = as.integer(to_num(apache_score_iva_model_missing_ind)),
  hosp_zero_abx_reporting_ind = as.integer(to_num(hosp_zero_abx_reporting_ind)),
  diabetes_model = as.integer(to_num(diabetes_model)),
  diabetes_model_missing_ind = as.integer(to_num(diabetes_model_missing_ind)),
  vent_model = as.integer(to_num(vent_model)),
  intubated_model = as.integer(to_num(intubated_model)),
  dialysis_model = as.integer(to_num(dialysis_model)),
  cirrhosis_model = as.integer(to_num(cirrhosis_model)),
  immunosuppression_model = as.integer(to_num(immunosuppression_model)),
  hepaticfailure_model = as.integer(to_num(hepaticfailure_model)),
  metastaticcancer_model = as.integer(to_num(metastaticcancer_model))
)]
time_weights[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  arm_id = as.integer(to_num(arm_id)),
  t_hour = as.integer(to_num(t_hour)),
  ipcw_t = to_num(ipcw_t)
)]

if (weights[, anyDuplicated(paste(.imp, clone_id))] > 0) {
  stop("Duplicate .imp + clone_id rows in final weights.", call. = FALSE)
}
if (time_weights[, anyDuplicated(paste(.imp, clone_id, t_hour))] > 0) {
  stop("Duplicate .imp + clone_id + t_hour rows in time weights.", call. = FALSE)
}

weights[, end_h := fifelse(is.na(artificial_censor_h), surv_hours_30d, pmin(surv_hours_30d, artificial_censor_h))]
weights[, event_observed_in_periods := death_30d_inhosp == 1L & surv_hours_30d <= end_h]
weights[, clone_row := .I]

period_def <- rbindlist(list(
  data.table(
    period_index = 1:24,
    period_type = "hour",
    time_start_h = 0:23,
    time_end_h = 1:24,
    time_axis = paste0("h", 0:23)
  ),
  data.table(
    period_index = 25:53,
    period_type = "day",
    time_start_h = seq(24, 29 * 24, by = 24),
    time_end_h = seq(48, 30 * 24, by = 24),
    time_axis = paste0("day", 2:30)
  )
))

build_imp_periods <- function(cb) {
  if (nrow(cb) == 0L) return(data.table())
  grid <- CJ(clone_row = cb$clone_row, period_index = period_def$period_index, sorted = FALSE)
  grid[period_def, `:=`(
    period_type = i.period_type,
    time_start_h = i.time_start_h,
    time_end_h = i.time_end_h,
    time_axis = i.time_axis
  ), on = "period_index"]
  grid[cb[, .(clone_row, end_h)], end_h := i.end_h, on = "clone_row"]
  grid <- grid[time_start_h < end_h]
  grid[, end_h := NULL]
  if (nrow(grid) == 0L) return(data.table())

  out <- merge(grid, cb, by = "clone_row", all.x = TRUE, sort = FALSE)
  out[, event_30d_period := as.integer(
    event_observed_in_periods &
      time_start_h < surv_hours_30d &
      surv_hours_30d <= time_end_h
  )]
  out
}

period_chunks <- lapply(split(weights, by = ".imp", keep.by = TRUE), build_imp_periods)
pp <- rbindlist(period_chunks, use.names = TRUE, fill = TRUE)
rm(period_chunks)
if (nrow(pp) == 0L) stop("No person-period rows were generated.", call. = FALSE)

time_map <- time_weights[, .(.imp, clone_id, time_start_h = t_hour, ipcw_period_current = ipcw_t)]
pp <- merge(pp, time_map, by = c(".imp", "clone_id", "time_start_h"), all.x = TRUE, sort = FALSE)
setorder(pp, .imp, clone_id, time_start_h, period_index)

pp[, ipcw_stabilized := ipcw_period_current]
pp[, ipcw_stabilized := nafill(ipcw_stabilized, type = "locf"), by = .(.imp, clone_id)]
pp[is.na(ipcw_stabilized), ipcw_stabilized := 1]
pp[, ipcw_stabilized_lag1 := shift(ipcw_stabilized, 1L, fill = 1), by = .(.imp, clone_id)]

trunc_limits <- weights[
  strategy_adherent_by_delay == TRUE & !is.na(final_ipcw),
  .(
    q01 = as.numeric(stats::quantile(final_ipcw, 0.01, names = FALSE)),
    q99 = as.numeric(stats::quantile(final_ipcw, 0.99, names = FALSE)),
    q05 = as.numeric(stats::quantile(final_ipcw, 0.05, names = FALSE)),
    q95 = as.numeric(stats::quantile(final_ipcw, 0.95, names = FALSE))
  ),
  by = .(.imp, arm)
]
pp <- merge(pp, trunc_limits, by = c(".imp", "arm"), all.x = TRUE, sort = FALSE)
pp[, ipcw_stabilized_trunc_p01_p99 := fifelse(
  is.na(q01) | is.na(q99), ipcw_stabilized, pmin(pmax(ipcw_stabilized, q01), q99)
)]
pp[, ipcw_stabilized_trunc_p05_p95 := fifelse(
  is.na(q05) | is.na(q95), ipcw_stabilized, pmin(pmax(ipcw_stabilized, q05), q95)
)]

event_by_clone <- pp[, .(
  events_per_clone = sum(event_30d_period, na.rm = TRUE)
), by = .(.imp, arm, arm_id, clone_id, patientunitstayid)]

if (event_by_clone[, any(events_per_clone > 1L)]) {
  print(event_by_clone[events_per_clone > 1L])
  stop("A clone has more than one event period.", call. = FALSE)
}
if (pp[, any(is.na(ipcw_stabilized) | !is.finite(ipcw_stabilized) | ipcw_stabilized <= 0)]) {
  stop("Missing, non-finite, or non-positive IPCW remains in person-period table.", call. = FALSE)
}

zero_period <- weights[!clone_row %in% pp$clone_row, .(
  zero_period_clones = .N
), by = .(.imp, arm, arm_id)]

diag_by_arm <- pp[, .(
  rows = .N,
  clones = uniqueN(clone_id),
  unique_stays = uniqueN(patientunitstayid),
  period_event_sum = sum(event_30d_period, na.rm = TRUE),
  weighted_event_sum = sum(event_30d_period * ipcw_stabilized, na.rm = TRUE),
  min_time_start_h = min(time_start_h, na.rm = TRUE),
  max_time_end_h = max(time_end_h, na.rm = TRUE),
  missing_weight_rows = sum(is.na(ipcw_stabilized)),
  mean_weight = mean(ipcw_stabilized, na.rm = TRUE),
  max_weight = max(ipcw_stabilized, na.rm = TRUE)
), by = .(.imp, arm, arm_id)]

clone_event_diag <- event_by_clone[, .(
  clone_with_event = sum(events_per_clone == 1L),
  clone_with_gt1_event = sum(events_per_clone > 1L)
), by = .(.imp, arm, arm_id)]

diag_by_arm <- merge(diag_by_arm, clone_event_diag, by = c(".imp", "arm", "arm_id"), all.x = TRUE, sort = FALSE)
diag_by_arm <- merge(diag_by_arm, zero_period, by = c(".imp", "arm", "arm_id"), all.x = TRUE, sort = FALSE)
diag_by_arm[is.na(zero_period_clones), zero_period_clones := 0L]
diag_by_arm[, section := "by_arm"]

diag_overall <- pp[, .(
  rows = .N,
  clones = uniqueN(clone_id),
  unique_stays = uniqueN(patientunitstayid),
  period_event_sum = sum(event_30d_period, na.rm = TRUE),
  weighted_event_sum = sum(event_30d_period * ipcw_stabilized, na.rm = TRUE),
  min_time_start_h = min(time_start_h, na.rm = TRUE),
  max_time_end_h = max(time_end_h, na.rm = TRUE),
  missing_weight_rows = sum(is.na(ipcw_stabilized)),
  mean_weight = mean(ipcw_stabilized, na.rm = TRUE),
  max_weight = max(ipcw_stabilized, na.rm = TRUE)
), by = .imp]
overall_events <- event_by_clone[, .(
  clone_with_event = sum(events_per_clone == 1L),
  clone_with_gt1_event = sum(events_per_clone > 1L)
), by = .imp]
overall_zero <- weights[!clone_row %in% pp$clone_row, .(zero_period_clones = .N), by = .imp]
diag_overall <- merge(diag_overall, overall_events, by = ".imp", all.x = TRUE, sort = FALSE)
diag_overall <- merge(diag_overall, overall_zero, by = ".imp", all.x = TRUE, sort = FALSE)
diag_overall[is.na(zero_period_clones), zero_period_clones := 0L]
diag_overall[, `:=`(section = "overall", arm = "ALL", arm_id = NA_integer_)]

period_counts <- pp[, .(
  rows = .N,
  events = sum(event_30d_period, na.rm = TRUE),
  weighted_events = sum(event_30d_period * ipcw_stabilized, na.rm = TRUE)
), by = .(.imp, arm, arm_id, period_type, time_axis, time_start_h, time_end_h)]
period_counts[, section := "period_counts"]

diag_out <- rbindlist(list(diag_overall, diag_by_arm, period_counts), fill = TRUE)
setcolorder(diag_out, c("section", ".imp", "arm", "arm_id"))
setorder(diag_out, .imp, arm_id, section, time_start_h)

out_cols <- c(
  ".imp", "clone_id", "patientunitstayid", "uniquepid", "hospitalid",
  "arm", "arm_id", "arm_start_h", "arm_end_h",
  "period_index", "period_type", "time_start_h", "time_end_h", "time_axis",
  "event_30d_period", "death_30d_inhosp", "followup_30d_inhosp_days", "surv_hours_30d",
  "artificial_censor_h", "artificial_censor_reason",
  "strategy_adherent_by_delay", "natural_censor_before_arm_end", "empty_ipcw_product",
  "ipcw_period_current", "ipcw_stabilized", "ipcw_stabilized_lag1",
  "ipcw_stabilized_trunc_p01_p99", "ipcw_stabilized_trunc_p05_p95",
  "final_ipcw", "final_ipcw_trunc_p01_p99", "final_ipcw_trunc_p05_p95",
  "age_model", "apache_score_iva_model", "apache_score_iva_model_missing_ind",
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "diabetes_model_missing_ind",
  "vent_model", "intubated_model", "dialysis_model",
  "cirrhosis_model", "immunosuppression_model", "hepaticfailure_model", "metastaticcancer_model"
)
missing_out <- setdiff(out_cols, names(pp))
if (length(missing_out) > 0) {
  stop("Internal missing output columns: ", paste(missing_out, collapse = ", "), call. = FALSE)
}

fwrite(pp[, ..out_cols], out_pp)
fwrite(diag_out, out_diag)

report_lines <- c(
  "eICU formal_v1 step 09 arm2 30-day person-period table",
  paste("weights_file:", weights_file),
  paste("time_weights_file:", time_weights_file),
  "",
  "overall diagnostics:",
  capture.output(print(diag_overall)),
  "",
  "by-arm diagnostics:",
  capture.output(print(diag_by_arm)),
  "",
  "notes:",
  "Outcome periods stop at artificial_censor_h when a clone deviates from the assigned strategy.",
  "Hourly IPCW is carried forward within clone after hour 12; initial missing IPCW is set to 1.",
  "No cross-patient weight fill is used."
)
writeLines(report_lines, out_report)

message("Wrote person-period table: ", out_pp)
message("Wrote diagnostics: ", out_diag)
message("Wrote report: ", out_report)
