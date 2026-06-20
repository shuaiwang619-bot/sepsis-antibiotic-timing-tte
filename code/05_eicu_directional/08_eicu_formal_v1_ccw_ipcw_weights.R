# eICU formal_v1 step 08: arm2 IPCW for artificial censoring
# MIMIC counterpart: 08_formal_v1_ccw_ipcw_weights.R
#
# Scope:
#   Estimate arm-specific stabilized inverse probability of artificial
#   censoring weights for the exploratory eICU arm2 CCW/IPCW supplement.

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
hourly_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_hourly_input_m5.csv")
}
clone_file <- if (length(args) >= 3) {
  args[3]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_clones_m5.csv")
}

out_dir <- file.path(base_dir, "outputs", "ccw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_weights <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_ipcw_final_weights_m5.csv")
out_time_weights <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_ipcw_time_weights_m5.csv")
out_diag <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_ipcw_weight_diagnostics.csv")
out_model_diag <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_ipcw_model_diagnostics.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_ipcw_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

clip_prob <- function(p, eps = 1e-6) {
  p <- as.numeric(p)
  pmin(pmax(p, eps), 1 - eps)
}

weight_summary <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0L) {
    return(data.table(
      n = 0L, mean = NA_real_, sd = NA_real_, min = NA_real_,
      p01 = NA_real_, p05 = NA_real_, p25 = NA_real_, median = NA_real_,
      p75 = NA_real_, p95 = NA_real_, p99 = NA_real_, max = NA_real_,
      gt10 = NA_integer_, gt20 = NA_integer_, gt50 = NA_integer_
    ))
  }
  data.table(
    n = length(x),
    mean = mean(x),
    sd = stats::sd(x),
    min = min(x),
    p01 = as.numeric(stats::quantile(x, 0.01, names = FALSE)),
    p05 = as.numeric(stats::quantile(x, 0.05, names = FALSE)),
    p25 = as.numeric(stats::quantile(x, 0.25, names = FALSE)),
    median = stats::median(x),
    p75 = as.numeric(stats::quantile(x, 0.75, names = FALSE)),
    p95 = as.numeric(stats::quantile(x, 0.95, names = FALSE)),
    p99 = as.numeric(stats::quantile(x, 0.99, names = FALSE)),
    max = max(x),
    gt10 = sum(x > 10),
    gt20 = sum(x > 20),
    gt50 = sum(x > 50)
  )
}

fit_glm_prob <- function(formula, data, label) {
  fit <- suppressWarnings(stats::glm(formula, data = data, family = stats::binomial()))
  if (!isTRUE(fit$converged)) {
    warning(label, " did not report glm convergence.")
  }
  p <- clip_prob(stats::predict(fit, type = "response"))
  if (any(!is.finite(p) | is.na(p))) {
    stop("Non-finite fitted probabilities in ", label, call. = FALSE)
  }
  list(prob = p, fit = fit)
}

has_variation <- function(x) {
  if (is.factor(x)) return(nlevels(droplevels(x)) > 1L)
  xx <- x[!is.na(x)]
  length(unique(xx)) > 1L
}

make_den_formula <- function(dat) {
  term_map <- list(
    "factor(t_hour)" = "t_hour",
    "age_model" = "age_model",
    "apache_score_iva_model" = "apache_score_iva_model",
    "apache_score_iva_model_missing_ind" = "apache_score_iva_model_missing_ind",
    "gender_factor" = "gender_factor",
    "ethnicity_group" = "ethnicity_group",
    "unit_admit_source_group" = "unit_admit_source_group",
    "unit_type_group" = "unit_type_group",
    "hosp_window_volume_bucket_factor" = "hosp_window_volume_bucket_factor",
    "hosp_zero_abx_reporting_ind" = "hosp_zero_abx_reporting_ind",
    "diabetes_model" = "diabetes_model",
    "diabetes_model_missing_ind" = "diabetes_model_missing_ind"
  )
  keep <- names(term_map)[vapply(term_map, function(v) has_variation(dat[[v]]), logical(1))]
  if (!"factor(t_hour)" %in% keep) keep <- c("factor(t_hour)", keep)
  stats::as.formula(paste("artificial_censor_event ~", paste(keep, collapse = " + ")))
}

stop_if_missing(hourly_file, "Step 06 hourly input")
stop_if_missing(clone_file, "Step 07 clone file")

hourly <- fread(hourly_file, showProgress = FALSE)
clones <- fread(clone_file, showProgress = FALSE)

need_hourly <- c(".imp", "patientunitstayid", "t_hour", "still_observed_i")
need_clone <- c(
  ".imp", "clone_id", "patientunitstayid", "uniquepid", "hospitalid",
  "arm", "arm_id", "arm_start_h", "arm_end_h",
  "death_30d_inhosp", "followup_30d_inhosp_days", "surv_hours_30d",
  "arm2_abx_timing", "arm2_late_3_12h",
  "first_abx_delay_hours_from_unit",
  "artificial_censor_h", "artificial_censor_reason",
  "strategy_adherent_by_delay", "natural_censor_before_arm_end",
  "age_model", "apache_score_iva_model", "apache_score_iva_model_missing_ind",
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "diabetes_model_missing_ind",
  "vent_model", "intubated_model", "dialysis_model",
  "cirrhosis_model", "immunosuppression_model", "hepaticfailure_model", "metastaticcancer_model"
)
missing_hourly <- setdiff(need_hourly, names(hourly))
missing_clone <- setdiff(need_clone, names(clones))
if (length(missing_hourly) > 0) stop("Hourly input missing columns: ", paste(missing_hourly, collapse = ", "), call. = FALSE)
if (length(missing_clone) > 0) stop("Clone file missing columns: ", paste(missing_clone, collapse = ", "), call. = FALSE)

hourly[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  t_hour = as.integer(to_num(t_hour)),
  still_observed_i = as.integer(to_num(still_observed_i))
)]
clones[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  hospitalid = as.integer(to_num(hospitalid)),
  arm_id = as.integer(to_num(arm_id)),
  arm_start_h = as.integer(to_num(arm_start_h)),
  arm_end_h = as.integer(to_num(arm_end_h)),
  death_30d_inhosp = as.integer(to_num(death_30d_inhosp)),
  followup_30d_inhosp_days = to_num(followup_30d_inhosp_days),
  surv_hours_30d = to_num(surv_hours_30d),
  first_abx_delay_hours_from_unit = to_num(first_abx_delay_hours_from_unit),
  artificial_censor_h = to_num(artificial_censor_h),
  strategy_adherent_by_delay = as.logical(strategy_adherent_by_delay),
  natural_censor_before_arm_end = as.integer(to_num(natural_censor_before_arm_end)),
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

if (hourly[, anyDuplicated(paste(.imp, patientunitstayid, t_hour))] > 0) {
  stop("Duplicate .imp + patientunitstayid + t_hour rows in hourly input.", call. = FALSE)
}
if (clones[, anyDuplicated(paste(.imp, clone_id))] > 0) {
  stop("Duplicate .imp + clone_id rows in clone file.", call. = FALSE)
}

model_dat <- hourly[clones, on = c(".imp", "patientunitstayid"), allow.cartesian = TRUE, nomatch = 0]
model_dat <- model_dat[
  still_observed_i == 1L &
    t_hour <= arm_end_h &
    (is.na(artificial_censor_h) | t_hour <= artificial_censor_h)
]
model_dat[, artificial_censor_event := as.integer(!is.na(artificial_censor_h) & t_hour == artificial_censor_h)]
setorder(model_dat, .imp, arm_id, patientunitstayid, t_hour)

factor_cols <- c(
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor"
)
for (cc in factor_cols) {
  model_dat[[cc]] <- factor(model_dat[[cc]])
}
model_dat[, `:=`(
  diabetes_model = factor(diabetes_model, levels = c(0, 1)),
  hosp_zero_abx_reporting_ind = factor(hosp_zero_abx_reporting_ind, levels = c(0, 1)),
  apache_score_iva_model_missing_ind = factor(apache_score_iva_model_missing_ind, levels = c(0, 1)),
  diabetes_model_missing_ind = factor(diabetes_model_missing_ind, levels = c(0, 1))
)]

required_model_cols <- c(
  "artificial_censor_event", "t_hour", "age_model", "apache_score_iva_model",
  "apache_score_iva_model_missing_ind", "gender_factor", "ethnicity_group",
  "unit_admit_source_group", "unit_type_group", "hosp_window_volume_bucket_factor",
  "hosp_zero_abx_reporting_ind", "diabetes_model", "diabetes_model_missing_ind"
)
model_na <- model_dat[, lapply(.SD, function(x) sum(is.na(x))), by = .imp, .SDcols = required_model_cols]
if (model_na[, any(rowSums(.SD) > 0), .SDcols = required_model_cols]) {
  print(model_na)
  stop("NA remains in IPCW censor model columns.", call. = FALSE)
}

num_formula <- artificial_censor_event ~ factor(t_hour)
den_formula_template <- artificial_censor_event ~
  factor(t_hour) +
  age_model +
  apache_score_iva_model +
  apache_score_iva_model_missing_ind +
  gender_factor +
  ethnicity_group +
  unit_admit_source_group +
  unit_type_group +
  hosp_window_volume_bucket_factor +
  hosp_zero_abx_reporting_ind +
  diabetes_model +
  diabetes_model_missing_ind

model_counts <- model_dat[, .(
  rows = .N,
  clone_ids = uniqueN(clone_id),
  events = sum(artificial_censor_event),
  event_rate = mean(artificial_censor_event)
), by = .(.imp, arm, arm_id)][order(.imp, arm_id)]

if (model_counts[, any(events == 0 | events == rows)]) {
  print(model_counts)
  stop("No artificial-censoring variation in at least one .imp + arm model.", call. = FALSE)
}

model_dat[, p_num_censor := NA_real_]
model_dat[, p_den_censor := NA_real_]

fit_log <- list()
for (ii in sort(unique(model_dat$.imp))) {
  for (aa in c("0-3h", "3-12h")) {
    idx <- which(model_dat$.imp == ii & model_dat$arm == aa)
    dat <- droplevels(model_dat[idx])
    label <- paste0(".imp=", ii, ", arm=", aa)

    den_formula <- make_den_formula(dat)

    p_num <- fit_glm_prob(num_formula, dat, paste0(label, " numerator"))
    p_den <- fit_glm_prob(den_formula, dat, paste0(label, " denominator"))

    model_dat[idx, p_num_censor := p_num$prob]
    model_dat[idx, p_den_censor := p_den$prob]

    fit_log[[length(fit_log) + 1L]] <- data.table(
      .imp = ii,
      arm = aa,
      rows = nrow(dat),
      events = sum(dat$artificial_censor_event),
      numerator_converged = isTRUE(p_num$fit$converged),
      denominator_converged = isTRUE(p_den$fit$converged),
      denominator_rank = p_den$fit$rank,
      denominator_coef_n = length(stats::coef(p_den$fit)),
      numerator_formula = paste(deparse(num_formula, width.cutoff = 500L), collapse = " "),
      denominator_formula = paste(deparse(den_formula, width.cutoff = 500L), collapse = " ")
    )
  }
}
fit_log <- rbindlist(fit_log, fill = TRUE)

model_dat[, surv_ratio := (1 - p_num_censor) / (1 - p_den_censor)]
model_dat[, ipcw_t := cumprod(surv_ratio), by = .(.imp, arm, patientunitstayid)]

last_weight_rows <- model_dat[
  order(.imp, arm_id, patientunitstayid, t_hour),
  .(
    last_weight_hour = t_hour[.N],
    ipcw_at_last_model_row = ipcw_t[.N],
    censor_model_rows = .N,
    censor_event_in_model = max(artificial_censor_event),
    p_num_censor_last = p_num_censor[.N],
    p_den_censor_last = p_den_censor[.N]
  ),
  by = .(.imp, clone_id)
]

weights <- merge(clones, last_weight_rows, by = c(".imp", "clone_id"), all.x = TRUE, sort = FALSE)
weights[, weight_reached_arm_end := as.integer(!is.na(last_weight_hour) & last_weight_hour >= arm_end_h)]
weights[, empty_ipcw_product := as.integer(strategy_adherent_by_delay & is.na(ipcw_at_last_model_row))]
weights[strategy_adherent_by_delay & empty_ipcw_product == 1L, ipcw_at_last_model_row := 1]
weights[, final_ipcw := fifelse(strategy_adherent_by_delay, ipcw_at_last_model_row, NA_real_)]
weights[, final_ipcw_trunc_p01_p99 := final_ipcw]
weights[, final_ipcw_trunc_p05_p95 := final_ipcw]

for (ii in sort(unique(weights$.imp))) {
  for (aa in c("0-3h", "3-12h")) {
    idx <- which(weights$.imp == ii & weights$arm == aa & !is.na(weights$final_ipcw))
    x <- weights$final_ipcw[idx]
    if (length(x) > 0L) {
      q01 <- as.numeric(stats::quantile(x, 0.01, names = FALSE))
      q99 <- as.numeric(stats::quantile(x, 0.99, names = FALSE))
      q05 <- as.numeric(stats::quantile(x, 0.05, names = FALSE))
      q95 <- as.numeric(stats::quantile(x, 0.95, names = FALSE))
      weights$final_ipcw_trunc_p01_p99[idx] <- pmin(pmax(x, q01), q99)
      weights$final_ipcw_trunc_p05_p95[idx] <- pmin(pmax(x, q05), q95)
    }
  }
}

weight_missing_diag <- weights[, .(
  clones = .N,
  adherent = sum(strategy_adherent_by_delay),
  final_weight_nonmissing = sum(strategy_adherent_by_delay & !is.na(final_ipcw)),
  adherent_missing_final_weight = sum(strategy_adherent_by_delay & is.na(final_ipcw)),
  adherent_not_reached_arm_end = sum(strategy_adherent_by_delay & weight_reached_arm_end == 0L, na.rm = TRUE),
  natural_censor_before_arm_end = sum(natural_censor_before_arm_end == 1L, na.rm = TRUE),
  empty_ipcw_product = sum(empty_ipcw_product == 1L, na.rm = TRUE)
), by = .(.imp, arm, arm_id)][order(.imp, arm_id)]
if (weight_missing_diag[, any(adherent_missing_final_weight > 0)]) {
  print(weight_missing_diag)
  stop("Some adherent clones lack final IPCW.", call. = FALSE)
}

diag_none <- weights[
  strategy_adherent_by_delay == TRUE,
  weight_summary(final_ipcw),
  by = .(.imp, arm, arm_id)
][, truncation := "none"]
diag_trunc_01_99 <- weights[
  strategy_adherent_by_delay == TRUE,
  weight_summary(final_ipcw_trunc_p01_p99),
  by = .(.imp, arm, arm_id)
][, truncation := "p01_p99"]
diag_trunc_05_95 <- weights[
  strategy_adherent_by_delay == TRUE,
  weight_summary(final_ipcw_trunc_p05_p95),
  by = .(.imp, arm, arm_id)
][, truncation := "p05_p95"]
diag_out <- rbindlist(list(diag_none, diag_trunc_01_99, diag_trunc_05_95), fill = TRUE)

weights_out <- weights[, .(
  .imp, clone_id, patientunitstayid, uniquepid, hospitalid,
  arm, arm_id, arm_start_h, arm_end_h,
  death_30d_inhosp, followup_30d_inhosp_days, surv_hours_30d,
  arm2_abx_timing, arm2_late_3_12h,
  first_abx_delay_hours_from_unit,
  artificial_censor_h, artificial_censor_reason,
  strategy_adherent_by_delay, natural_censor_before_arm_end,
  last_weight_hour, weight_reached_arm_end, empty_ipcw_product,
  final_ipcw, final_ipcw_trunc_p01_p99, final_ipcw_trunc_p05_p95,
  age_model, apache_score_iva_model, apache_score_iva_model_missing_ind,
  gender_factor, ethnicity_group, unit_admit_source_group, unit_type_group,
  hosp_window_volume_bucket_factor, hosp_zero_abx_reporting_ind,
  diabetes_model, diabetes_model_missing_ind,
  vent_model, intubated_model, dialysis_model,
  cirrhosis_model, immunosuppression_model, hepaticfailure_model, metastaticcancer_model
)]

time_weights_out <- model_dat[, .(
  .imp, clone_id, patientunitstayid, arm, arm_id, t_hour,
  artificial_censor_event, p_num_censor, p_den_censor, surv_ratio, ipcw_t
)]

fwrite(weights_out, out_weights)
fwrite(time_weights_out, out_time_weights)
fwrite(diag_out, out_diag)
fwrite(rbindlist(list(
  cbind(section = "model_counts", model_counts),
  cbind(section = "fit_log", fit_log),
  cbind(section = "weight_missing", weight_missing_diag),
  cbind(section = "model_na", model_na)
), fill = TRUE), out_model_diag)

report_lines <- c(
  "eICU formal_v1 step 08 arm2 IPCW",
  paste("hourly_file:", hourly_file),
  paste("clone_file:", clone_file),
  "",
  "censor model counts:",
  capture.output(print(model_counts)),
  "",
  "weight missing diagnostics:",
  capture.output(print(weight_missing_diag)),
  "",
  "weight diagnostics:",
  capture.output(print(diag_out)),
  "",
  "fit log:",
  capture.output(print(fit_log)),
  "",
  "notes:",
  "Numerator model: artificial censoring ~ factor(t_hour).",
  paste("Denominator template:", paste(deparse(den_formula_template, width.cutoff = 500L), collapse = " ")),
  "Denominator models automatically drop predictors with no within-arm variation before fitting.",
  "Weights are stabilized and arm-specific. Non-adherent clones have NA final weights but retain time-varying weights until artificial censoring."
)
writeLines(report_lines, out_report)

message("Wrote final weights: ", out_weights)
message("Wrote time weights: ", out_time_weights)
message("Wrote diagnostics: ", out_diag)
message("Wrote report: ", out_report)
