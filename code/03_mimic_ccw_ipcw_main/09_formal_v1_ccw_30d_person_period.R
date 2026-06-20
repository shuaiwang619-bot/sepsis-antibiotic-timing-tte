# formal_v1 CCW 30-day person-period outcome table
# Scope: build the pooled-logistic outcome data from cloned strategies.
#
# Time axis:
#   h0-h23      = hourly periods over the first 24 hours
#   day2-day30  = daily periods from 24 hours through 30 days
#
# Weights:
#   Hourly periods use the available cumulative IPCW from 08.
#   Missing later hours are carried forward within clone and are never filled
#   across patients. Initial missing weights use 1, which is the empty product.

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

weights_file <- file.path(base_dir, "formal_v1_ccw_ipcw_main_final_weights_m5.csv")
time_weights_file <- file.path(base_dir, "formal_v1_ccw_ipcw_main_time_weights_m5.csv")
wide_imp_file <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_model_ready_long.csv")

out_pp <- file.path(base_dir, "formal_v1_ccw_30d_person_period_m5.csv")
out_diag <- file.path(base_dir, "formal_v1_ccw_30d_person_period_diagnostics.csv")
out_report <- file.path(base_dir, "formal_v1_ccw_30d_person_period_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

library(data.table)

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

stop_if_missing(weights_file, "Clone-level IPCW file")
stop_if_missing(time_weights_file, "Time-varying IPCW file")
stop_if_missing(wide_imp_file, "Imputed wide table")

cat("formal_v1 CCW 30-day person-period outcome table\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Clone-level IPCW:", weights_file, "\n")
cat("Time-varying IPCW:", time_weights_file, "\n")
cat("Imputed wide table:", wide_imp_file, "\n")

need_weights <- c(
  ".imp", "clone_id", "stay_id", "subject_id", "hadm_id",
  "arm", "arm_id", "arm_start_h", "arm_end_h",
  "ccw_role_factor", "observed_abx_window", "delay_window_medical",
  "first_abx_delay_hours_clean", "artificial_censor_h",
  "artificial_censor_reason", "strategy_adherent_by_delay",
  "natural_censor_before_arm_end", "empty_ipcw_product",
  "death_30d", "fu_time_30d_days", "final_ipcw",
  "final_ipcw_trunc_p01_p99", "final_ipcw_trunc_p05_p95"
)
need_time <- c(".imp", "clone_id", "stay_id", "arm", "arm_id", "t_hour", "ipcw_t")
need_wide <- c(
  ".imp", "stay_id",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing", "shock_t0_b_factor", "shock_t0_b_num_model",
  "meanbp_min_t0_missing", "meanbp_min_t0_extreme_or_invalid",
  "sofa_total_t0_imp", "meanbp_min_t0_imp_sens"
)

weights_header <- names(fread(weights_file, nrows = 0, showProgress = FALSE))
time_header <- names(fread(time_weights_file, nrows = 0, showProgress = FALSE))
wide_header <- names(fread(wide_imp_file, nrows = 0, showProgress = FALSE))
missing_weights <- setdiff(need_weights, weights_header)
missing_time <- setdiff(need_time, time_header)
missing_wide <- setdiff(need_wide, wide_header)
if (length(missing_weights) > 0) stop("Weights missing columns: ", paste(missing_weights, collapse = ", "), call. = FALSE)
if (length(missing_time) > 0) stop("Time weights missing columns: ", paste(missing_time, collapse = ", "), call. = FALSE)
if (length(missing_wide) > 0) stop("Wide table missing columns: ", paste(missing_wide, collapse = ", "), call. = FALSE)

weights <- fread(weights_file, select = need_weights, showProgress = FALSE)
time_weights <- fread(time_weights_file, select = need_time, showProgress = FALSE)
wide <- fread(wide_imp_file, select = need_wide, showProgress = FALSE)

weights[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  arm_id = as.integer(arm_id),
  arm_start_h = as.integer(as_num(arm_start_h)),
  arm_end_h = as.integer(as_num(arm_end_h)),
  first_abx_delay_hours_clean = as_num(first_abx_delay_hours_clean),
  artificial_censor_h = as_num(artificial_censor_h),
  strategy_adherent_by_delay = as.logical(strategy_adherent_by_delay),
  natural_censor_before_arm_end = as.integer(as_num(natural_censor_before_arm_end)),
  empty_ipcw_product = as.integer(as_num(empty_ipcw_product)),
  death_30d = as.integer(as_num(death_30d)),
  fu_time_30d_days = as_num(fu_time_30d_days),
  final_ipcw = as_num(final_ipcw),
  final_ipcw_trunc_p01_p99 = as_num(final_ipcw_trunc_p01_p99),
  final_ipcw_trunc_p05_p95 = as_num(final_ipcw_trunc_p05_p95)
)]
time_weights[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  arm_id = as.integer(arm_id),
  t_hour = as.integer(as_num(t_hour)),
  ipcw_t = as_num(ipcw_t)
)]
wide[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  age_at_icu_model = as_num(age_at_icu_model),
  charlson_comorbidity_index_model = as_num(charlson_comorbidity_index_model),
  sofa_total_t0_missing = as.integer(as_num(sofa_total_t0_missing)),
  shock_t0_b_num_model = as.integer(as_num(shock_t0_b_num_model)),
  meanbp_min_t0_missing = as.integer(as_num(meanbp_min_t0_missing)),
  meanbp_min_t0_extreme_or_invalid = as.integer(as_num(meanbp_min_t0_extreme_or_invalid)),
  sofa_total_t0_imp = as_num(sofa_total_t0_imp),
  meanbp_min_t0_imp_sens = as_num(meanbp_min_t0_imp_sens)
)]

weights <- weights[.imp %in% 1:5]
time_weights <- time_weights[.imp %in% 1:5]
wide <- wide[.imp %in% 1:5]

if (weights[, anyDuplicated(paste(.imp, clone_id))] > 0) {
  stop("Duplicate .imp + clone_id rows in clone-level weights.", call. = FALSE)
}
if (wide[, anyDuplicated(paste(.imp, stay_id))] > 0) {
  stop("Duplicate .imp + stay_id rows in imputed wide table.", call. = FALSE)
}
if (time_weights[, anyDuplicated(paste(.imp, clone_id, t_hour))] > 0) {
  stop("Duplicate .imp + clone_id + t_hour rows in time-varying weights.", call. = FALSE)
}

clone_base <- merge(weights, wide, by = c(".imp", "stay_id"), all.x = TRUE, sort = FALSE)

baseline_need <- setdiff(need_wide, c(".imp", "stay_id"))
baseline_na <- clone_base[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = baseline_need]
if (baseline_na[, rowSums(.SD) > 0]) {
  print(baseline_na)
  stop("Baseline covariates contain NA after wide-table imputation.", call. = FALSE)
}

clone_base[, fu_days_for_period := fifelse(is.na(fu_time_30d_days), 30, fu_time_30d_days)]
clone_base[, surv_hours := pmin(30 * 24, pmax(0, fu_days_for_period * 24))]
clone_base[, end_h := fifelse(is.na(artificial_censor_h), surv_hours, pmin(surv_hours, artificial_censor_h))]
clone_base[, event_observed_in_periods := death_30d == 1L & surv_hours <= end_h]
clone_base[, clone_row := .I]

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
  grid <- CJ(
    clone_row = cb$clone_row,
    period_index = period_def$period_index,
    sorted = FALSE
  )
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
      time_start_h < surv_hours &
      surv_hours <= time_end_h
  )]
  out
}

print_section("1. Building Period Rows")
period_chunks <- lapply(split(clone_base, by = ".imp", keep.by = TRUE), build_imp_periods)
pp <- rbindlist(period_chunks, use.names = TRUE, fill = TRUE)
rm(period_chunks)

if (nrow(pp) == 0L) stop("No person-period rows were generated.", call. = FALSE)

time_map <- time_weights[, .(
  .imp,
  clone_id,
  time_start_h = t_hour,
  ipcw_period_current = ipcw_t
)]
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
  is.na(q01) | is.na(q99),
  ipcw_stabilized,
  pmin(pmax(ipcw_stabilized, q01), q99)
)]
pp[, ipcw_stabilized_trunc_p05_p95 := fifelse(
  is.na(q05) | is.na(q95),
  ipcw_stabilized,
  pmin(pmax(ipcw_stabilized, q05), q95)
)]

event_by_clone <- pp[, .(
  events_per_clone = sum(event_30d_period, na.rm = TRUE)
), by = .(.imp, arm, arm_id, clone_id, stay_id)]

if (event_by_clone[, any(events_per_clone > 1L)]) {
  print(event_by_clone[events_per_clone > 1L])
  stop("A clone has more than one event period.", call. = FALSE)
}
if (pp[, any(is.na(ipcw_stabilized))]) {
  stop("Missing carried-forward IPCW remains in person-period table.", call. = FALSE)
}

zero_period <- clone_base[!clone_row %in% pp$clone_row, .(
  zero_period_clones = .N
), by = .(.imp, arm, arm_id)]

diag_by_arm <- pp[, .(
  rows = .N,
  clones = uniqueN(clone_id),
  unique_stay = uniqueN(stay_id),
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
  unique_stay = uniqueN(stay_id),
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
overall_zero <- clone_base[!clone_row %in% pp$clone_row, .(
  zero_period_clones = .N
), by = .imp]
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

diag_out <- rbindlist(list(
  diag_overall,
  diag_by_arm,
  period_counts
), fill = TRUE)
setcolorder(diag_out, c("section", ".imp", "arm", "arm_id"))
setorder(diag_out, .imp, arm_id, section, time_start_h)

out_cols <- c(
  ".imp", "clone_id", "stay_id", "subject_id", "hadm_id",
  "arm", "arm_id", "arm_start_h", "arm_end_h",
  "period_index", "period_type", "time_start_h", "time_end_h", "time_axis",
  "event_30d_period", "death_30d", "fu_time_30d_days",
  "artificial_censor_h", "artificial_censor_reason",
  "strategy_adherent_by_delay", "natural_censor_before_arm_end", "empty_ipcw_product",
  "ipcw_period_current", "ipcw_stabilized", "ipcw_stabilized_lag1",
  "ipcw_stabilized_trunc_p01_p99", "ipcw_stabilized_trunc_p05_p95",
  "final_ipcw", "final_ipcw_trunc_p01_p99", "final_ipcw_trunc_p05_p95",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing", "shock_t0_b_factor", "shock_t0_b_num_model",
  "meanbp_min_t0_missing", "meanbp_min_t0_extreme_or_invalid",
  "sofa_total_t0_imp", "meanbp_min_t0_imp_sens"
)
missing_out_cols <- setdiff(out_cols, names(pp))
if (length(missing_out_cols) > 0) {
  stop("Internal missing output columns: ", paste(missing_out_cols, collapse = ", "), call. = FALSE)
}

print_section("2. Diagnostics")
print(diag_overall[order(.imp)])
print(diag_by_arm[order(.imp, arm_id)])

fwrite(pp[, ..out_cols], out_pp)
fwrite(diag_out, out_diag)

cat("=== formal_v1 CCW 30-day person-period outcome table ===\n", file = out_report)
cat("clone_weights:", weights_file, "\n", file = out_report, append = TRUE)
cat("time_weights:", time_weights_file, "\n", file = out_report, append = TRUE)
cat("wide_imputed:", wide_imp_file, "\n", file = out_report, append = TRUE)
cat("output:", out_pp, "\n", file = out_report, append = TRUE)
cat("rows:", nrow(pp), "\n", file = out_report, append = TRUE)
cat("clones:", pp[, uniqueN(clone_id)], "\n", file = out_report, append = TRUE)
cat("unique_stay:", pp[, uniqueN(stay_id)], "\n", file = out_report, append = TRUE)
cat("events:", pp[, sum(event_30d_period, na.rm = TRUE)], "\n", file = out_report, append = TRUE)
cat("weighted_events:", pp[, sum(event_30d_period * ipcw_stabilized, na.rm = TRUE)], "\n", file = out_report, append = TRUE)
cat("missing_weight_rows:", pp[, sum(is.na(ipcw_stabilized))], "\n", file = out_report, append = TRUE)
cat("clone_with_gt1_event:", event_by_clone[, sum(events_per_clone > 1L)], "\n", file = out_report, append = TRUE)
cat("zero_period_clones:", clone_base[!clone_row %in% pp$clone_row, .N], "\n", file = out_report, append = TRUE)
cat("time_axis_levels:", paste(c(paste0("h", 0:23), paste0("day", 2:30)), collapse = ","), "\n\n", file = out_report, append = TRUE)

cat("overall_diagnostics:\n", file = out_report, append = TRUE)
capture.output(print(diag_overall[order(.imp)]), file = out_report, append = TRUE)
cat("\nby_arm_diagnostics:\n", file = out_report, append = TRUE)
capture.output(print(diag_by_arm[order(.imp, arm_id)]), file = out_report, append = TRUE)

cat("\nnotes:\n", file = out_report, append = TRUE)
cat("Outcome periods stop at artificial_censor_h when a clone deviates from the assigned strategy.\n", file = out_report, append = TRUE)
cat("Hourly IPCW is carried forward within clone; initial missing IPCW is set to 1.\n", file = out_report, append = TRUE)
cat("No cross-patient weight fill is used.\n", file = out_report, append = TRUE)
cat("The table includes baseline model covariates from the MICE-completed wide table.\n", file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

print_section("3. Output")
cat("Wrote person-period table:", out_pp, "\n")
cat("Rows:", nrow(pp), "\n")
cat("Wrote diagnostics:", out_diag, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: 30-day person-period outcome table built.\n")
