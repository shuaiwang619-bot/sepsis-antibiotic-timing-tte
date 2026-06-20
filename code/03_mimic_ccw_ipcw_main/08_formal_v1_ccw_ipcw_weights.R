# formal_v1 CCW IPCW estimation
# Scope: estimate arm-specific stabilized inverse probability of artificial
# censoring weights for .imp = 1-5.
#
# Main IPCW model:
#   numerator   = artificial censoring ~ time
#   denominator = artificial censoring ~ time + baseline covariates
#                 + selected baseline-by-time interactions
#                 + lagged SOFA + lagged shock + their missing indicators
#
# This script uses rows through the artificial censoring event for censoring
# model estimation. Default output is compact clone-level final weights and
# diagnostics; no large cloned person-period table is written unless requested.

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
write_time_weight_output <- if (length(args) >= 2) {
  tolower(args[2]) %in% c("true", "t", "1", "yes", "y", "write")
} else {
  FALSE
}

long_ready_file <- file.path(base_dir, "formal_v1_cov_tvcov_ccw_model_ready_m5.csv")
wide_imp_file <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_model_ready_long.csv")
out_weights <- file.path(base_dir, "formal_v1_ccw_ipcw_main_final_weights_m5.csv")
out_diag <- file.path(base_dir, "formal_v1_ccw_ipcw_main_weight_diagnostics.csv")
out_model_diag <- file.path(base_dir, "formal_v1_ccw_ipcw_main_model_diagnostics.csv")
out_time_weights <- file.path(base_dir, "formal_v1_ccw_ipcw_main_time_weights_m5.csv")

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

clip_prob <- function(p, eps = 1e-6) {
  p <- as.numeric(p)
  pmin(pmax(p, eps), 1 - eps)
}

medical_delay_window <- function(delay) {
  out <- rep(NA_character_, length(delay))
  out[!is.na(delay) & delay >= 0 & delay <= 3] <- "0-3h"
  out[!is.na(delay) & delay > 3 & delay <= 6] <- "3-6h"
  out[!is.na(delay) & delay > 6 & delay <= 12] <- "6-12h"
  out[!is.na(delay) & delay > 12 & delay <= 72] <- ">=12h"
  out
}

censor_hour <- function(arm, delay) {
  out <- rep(NA_real_, length(delay))

  i <- arm == "0-3h" & (is.na(delay) | delay > 3)
  out[i] <- 3

  i <- arm == "3-6h" & !is.na(delay) & delay <= 3
  out[i] <- pmax(0, ceiling(delay[i]))
  i <- arm == "3-6h" & (is.na(delay) | delay > 6)
  out[i] <- 6

  i <- arm == "6-12h" & !is.na(delay) & delay <= 6
  out[i] <- pmax(0, ceiling(delay[i]))
  i <- arm == "6-12h" & (is.na(delay) | delay > 12)
  out[i] <- 12

  out
}

censor_reason <- function(arm, delay) {
  out <- rep("adherent", length(delay))
  out[arm == "0-3h" & (is.na(delay) | delay > 3)] <- "late_or_no_init"
  out[arm == "3-6h" & !is.na(delay) & delay <= 3] <- "early_init"
  out[arm == "3-6h" & (is.na(delay) | delay > 6)] <- "late_or_no_init"
  out[arm == "6-12h" & !is.na(delay) & delay <= 6] <- "early_init"
  out[arm == "6-12h" & (is.na(delay) | delay > 12)] <- "late_or_no_init"
  out
}

weight_summary <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
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
  p <- clip_prob(stats::predict(fit, type = "response"))
  bad <- sum(!is.finite(p) | is.na(p))
  if (bad > 0) {
    stop("Non-finite fitted probabilities in ", label, call. = FALSE)
  }
  p
}

stop_if_missing(long_ready_file, "MICE-aware long table")
stop_if_missing(wide_imp_file, "Imputed wide table")

cat("formal_v1 CCW IPCW weights\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("MICE-aware long table:", long_ready_file, "\n")
cat("Imputed wide table:", wide_imp_file, "\n")
cat("Write time-varying weight table:", write_time_weight_output, "\n")

need_long <- c(
  ".imp", "stay_id", "t_hour_clean", "still_observed_i",
  "first_abx_delay_hours_clean",
  "sofa_total_tvcov_fill", "shock_tvcov_b_fill",
  "sofa_total_tvcov_missing_model", "shock_tvcov_b_missing_model"
)
need_wide <- c(
  ".imp", "stay_id", "subject_id", "hadm_id", "ccw_role_factor",
  "abx_window_factor", "death_30d", "fu_time_30d_days",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing", "shock_t0_b_num_model", "sofa_total_t0_imp"
)

long_header <- names(fread(long_ready_file, nrows = 0, showProgress = FALSE))
wide_header <- names(fread(wide_imp_file, nrows = 0, showProgress = FALSE))
missing_long <- setdiff(need_long, long_header)
missing_wide <- setdiff(need_wide, wide_header)
if (length(missing_long) > 0) stop("Long missing columns: ", paste(missing_long, collapse = ", "), call. = FALSE)
if (length(missing_wide) > 0) stop("Wide missing columns: ", paste(missing_wide, collapse = ", "), call. = FALSE)

long <- fread(long_ready_file, select = need_long, showProgress = FALSE)
wide <- fread(wide_imp_file, select = need_wide, showProgress = FALSE)

long[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  t_hour_clean = as.integer(as_num(t_hour_clean)),
  still_observed_i = as.integer(still_observed_i),
  first_abx_delay_hours_clean = as_num(first_abx_delay_hours_clean),
  sofa_total_tvcov_fill = as_num(sofa_total_tvcov_fill),
  shock_tvcov_b_fill = as.integer(as_num(shock_tvcov_b_fill)),
  sofa_total_tvcov_missing_model = as.integer(as_num(sofa_total_tvcov_missing_model)),
  shock_tvcov_b_missing_model = as.integer(as_num(shock_tvcov_b_missing_model))
)]
wide[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  death_30d = as.integer(death_30d),
  fu_time_30d_days = as_num(fu_time_30d_days),
  age_at_icu_model = as_num(age_at_icu_model),
  charlson_comorbidity_index_model = as_num(charlson_comorbidity_index_model),
  sofa_total_t0_missing = as.integer(as_num(sofa_total_t0_missing)),
  shock_t0_b_num_model = as.integer(as_num(shock_t0_b_num_model)),
  sofa_total_t0_imp = as_num(sofa_total_t0_imp),
  sex_factor = factor(sex_factor),
  race_group = factor(race_group),
  admission_type_group = factor(admission_type_group)
)]

long <- long[.imp %in% 1:5]
wide <- wide[.imp %in% 1:5]

if (wide[, anyDuplicated(paste(.imp, stay_id))] > 0) {
  stop("Duplicate .imp + stay_id rows in imputed wide data.", call. = FALSE)
}
if (long[, anyDuplicated(paste(.imp, stay_id, t_hour_clean))] > 0) {
  stop("Duplicate .imp + stay_id + t_hour_clean rows in MICE-aware long data.", call. = FALSE)
}

main_na <- long[still_observed_i == 1L & t_hour_clean <= 12L, .(
  rows = .N,
  sofa_fill_na = sum(is.na(sofa_total_tvcov_fill)),
  shock_fill_na = sum(is.na(shock_tvcov_b_fill)),
  sofa_missing_flag_na = sum(is.na(sofa_total_tvcov_missing_model)),
  shock_missing_flag_na = sum(is.na(shock_tvcov_b_missing_model))
), by = .imp][order(.imp)]

if (main_na[, any(sofa_fill_na > 0 | shock_fill_na > 0 | sofa_missing_flag_na > 0 | shock_missing_flag_na > 0)]) {
  print(main_na)
  stop("Main IPCW design variables contain NA.", call. = FALSE)
}

delay_by_stay <- long[, .(
  first_abx_delay_hours_clean = first(first_abx_delay_hours_clean),
  delay_n_values = uniqueN(first_abx_delay_hours_clean)
), by = .(.imp, stay_id)]
if (delay_by_stay[delay_n_values > 1L, .N] > 0) {
  stop("first_abx_delay_hours_clean is not stable within .imp + stay_id.", call. = FALSE)
}
delay_by_stay[, delay_n_values := NULL]
delay_by_stay[, delay_window_medical := medical_delay_window(first_abx_delay_hours_clean)]

eligible_wide <- wide[ccw_role_factor %in% c("analyzable", "censor_noinit")]
eligible_wide <- merge(
  eligible_wide,
  delay_by_stay,
  by = c(".imp", "stay_id"),
  all.x = TRUE,
  sort = FALSE
)

boundary_check <- eligible_wide[ccw_role_factor == "analyzable", .(
  analyzable = .N,
  mismatch = sum(abx_window_factor != delay_window_medical, na.rm = TRUE),
  missing_delay_window = sum(is.na(delay_window_medical))
), by = .imp][order(.imp)]
if (boundary_check[, any(mismatch > 0 | missing_delay_window > 0)]) {
  print(boundary_check)
  stop("Observed antibiotic windows do not match medical delay windows.", call. = FALSE)
}

arm_def <- data.table(
  arm = c("0-3h", "3-6h", "6-12h"),
  arm_id = 1:3,
  arm_start_h = c(0L, 3L, 6L),
  arm_end_h = c(3L, 6L, 12L)
)

clones <- eligible_wide[, .(
  .imp,
  stay_id,
  subject_id,
  hadm_id,
  ccw_role_factor,
  observed_abx_window = abx_window_factor,
  delay_window_medical,
  first_abx_delay_hours_clean,
  death_30d,
  fu_time_30d_days,
  age_at_icu_model,
  sex_factor,
  race_group,
  admission_type_group,
  charlson_comorbidity_index_model,
  sofa_total_t0_missing,
  shock_t0_b_num_model,
  sofa_total_t0_imp
)]

clones[, tmp_cross_join := 1L]
arm_def[, tmp_cross_join := 1L]
clones <- merge(clones, arm_def, by = "tmp_cross_join", allow.cartesian = TRUE, sort = FALSE)
clones[, tmp_cross_join := NULL]
arm_def[, tmp_cross_join := NULL]

clones[, artificial_censor_h := censor_hour(arm, first_abx_delay_hours_clean)]
clones[, artificial_censor_reason := censor_reason(arm, first_abx_delay_hours_clean)]
clones[, strategy_adherent_by_delay := artificial_censor_reason == "adherent"]
clones[, clone_id := paste(.imp, stay_id, arm, sep = "_")]

long_model <- long[
  t_hour_clean <= 12L,
  .(
    .imp,
    stay_id,
    t_hour = t_hour_clean,
    still_observed_i,
    sofa_total_tvcov_fill,
    shock_tvcov_b_fill,
    sofa_total_tvcov_missing_model,
    shock_tvcov_b_missing_model
  )
]

model_dat <- long_model[clones, on = c(".imp", "stay_id"), allow.cartesian = TRUE, nomatch = 0]
model_dat <- model_dat[
  still_observed_i == 1L &
    t_hour <= arm_end_h &
    (is.na(artificial_censor_h) | t_hour <= artificial_censor_h)
]

model_dat[, artificial_censor_event := as.integer(!is.na(artificial_censor_h) & t_hour == artificial_censor_h)]
setorder(model_dat, .imp, arm_id, stay_id, t_hour)

model_dat[, sofa_lag1 := data.table::shift(sofa_total_tvcov_fill, 1L), by = .(.imp, arm, stay_id)]
model_dat[, shock_lag1 := data.table::shift(shock_tvcov_b_fill, 1L), by = .(.imp, arm, stay_id)]
model_dat[, sofa_missing_lag1 := data.table::shift(sofa_total_tvcov_missing_model, 1L), by = .(.imp, arm, stay_id)]
model_dat[, shock_missing_lag1 := data.table::shift(shock_tvcov_b_missing_model, 1L), by = .(.imp, arm, stay_id)]

model_dat[is.na(sofa_lag1), sofa_lag1 := sofa_total_tvcov_fill]
model_dat[is.na(shock_lag1), shock_lag1 := shock_tvcov_b_fill]
model_dat[is.na(sofa_missing_lag1), sofa_missing_lag1 := sofa_total_tvcov_missing_model]
model_dat[is.na(shock_missing_lag1), shock_missing_lag1 := shock_tvcov_b_missing_model]

required_model_cols <- c(
  "artificial_censor_event", "t_hour", "sofa_lag1", "shock_lag1",
  "sofa_missing_lag1", "shock_missing_lag1",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing", "shock_t0_b_num_model", "sofa_total_t0_imp"
)
model_na <- model_dat[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = required_model_cols]
if (model_na[, rowSums(.SD) > 0]) {
  print(model_na)
  stop("NA remains in IPCW model columns.", call. = FALSE)
}

num_formula <- artificial_censor_event ~ factor(t_hour)
den_formula <- artificial_censor_event ~
  factor(t_hour) +
  age_at_icu_model +
  sex_factor +
  race_group +
  admission_type_group +
  charlson_comorbidity_index_model +
  sofa_total_t0_imp +
  sofa_total_t0_missing +
  shock_t0_b_num_model +
  factor(t_hour):admission_type_group +
  factor(t_hour):shock_t0_b_num_model +
  factor(t_hour):sofa_total_t0_imp +
  factor(t_hour):age_at_icu_model +
  factor(t_hour):charlson_comorbidity_index_model +
  sofa_lag1 +
  shock_lag1 +
  sofa_missing_lag1 +
  shock_missing_lag1

print_section("1. Main IPCW Matrix Check")
print(main_na)

print_section("2. Censor Model Rows")
model_counts <- model_dat[, .(
  rows = .N,
  clone_ids = uniqueN(clone_id),
  events = sum(artificial_censor_event),
  event_rate = mean(artificial_censor_event)
), by = .(.imp, arm, arm_id)][order(.imp, arm_id)]
print(model_counts)

event_by_hour <- model_dat[, .(
  rows = .N,
  events = sum(artificial_censor_event)
), by = .(.imp, arm, arm_id, t_hour)][order(.imp, arm_id, t_hour)]

model_dat[, p_num_censor := NA_real_]
model_dat[, p_den_censor := NA_real_]

fit_log <- list()
for (ii in sort(unique(model_dat$.imp))) {
  for (aa in arm_def$arm) {
    idx <- which(model_dat$.imp == ii & model_dat$arm == aa)
    dat <- model_dat[idx]
    label <- paste0(".imp=", ii, ", arm=", aa)
    if (nrow(dat) == 0L || length(unique(dat$artificial_censor_event)) < 2L) {
      stop("Cannot fit censor model for ", label, ": no variation in censor event.", call. = FALSE)
    }

    p_num <- fit_glm_prob(num_formula, dat, paste0(label, " numerator"))
    p_den <- fit_glm_prob(den_formula, dat, paste0(label, " denominator"))

    model_dat[idx, p_num_censor := p_num]
    model_dat[idx, p_den_censor := p_den]
    fit_log[[length(fit_log) + 1L]] <- data.table(
      .imp = ii,
      arm = aa,
      rows = nrow(dat),
      events = sum(dat$artificial_censor_event),
      numerator_formula = paste(deparse(num_formula, width.cutoff = 500L), collapse = " "),
      denominator_formula = paste(deparse(den_formula, width.cutoff = 500L), collapse = " ")
    )
  }
}
fit_log <- rbindlist(fit_log, fill = TRUE)

model_dat[, surv_ratio := (1 - p_num_censor) / (1 - p_den_censor)]
model_dat[, ipcw_t := cumprod(surv_ratio), by = .(.imp, arm, stay_id)]

last_weight_rows <- model_dat[
  order(.imp, arm_id, stay_id, t_hour),
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

weights <- merge(
  clones,
  last_weight_rows,
  by = c(".imp", "clone_id"),
  all.x = TRUE,
  sort = FALSE
)
weights[, weight_reached_arm_end := as.integer(!is.na(last_weight_hour) & last_weight_hour >= arm_end_h)]
weights[, natural_censor_before_arm_end := as.integer(
  strategy_adherent_by_delay & (is.na(last_weight_hour) | last_weight_hour < arm_end_h)
)]
weights[, empty_ipcw_product := as.integer(strategy_adherent_by_delay & is.na(ipcw_at_last_model_row))]
weights[strategy_adherent_by_delay & empty_ipcw_product == 1L, ipcw_at_last_model_row := 1]
weights[, final_ipcw := fifelse(strategy_adherent_by_delay, ipcw_at_last_model_row, NA_real_)]
weights[, final_ipcw_trunc_p01_p99 := final_ipcw]
weights[, final_ipcw_trunc_p05_p95 := final_ipcw]

for (ii in sort(unique(weights$.imp))) {
  for (aa in arm_def$arm) {
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

diag_all <- rbindlist(list(diag_none, diag_trunc_01_99, diag_trunc_05_95), fill = TRUE)
setcolorder(diag_all, c(".imp", "arm", "arm_id", "truncation"))
setorder(diag_all, .imp, arm_id, truncation)

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
  stop("Some adherent clones lack a final IPCW after natural-censor handling.", call. = FALSE)
}

print_section("3. Final Weight Diagnostics")
print(diag_all)

print_section("4. Missing Final Weight Check")
print(weight_missing_diag)

weights_out <- weights[, .(
  .imp,
  clone_id,
  stay_id,
  subject_id,
  hadm_id,
  arm,
  arm_id,
  arm_start_h,
  arm_end_h,
  ccw_role_factor,
  observed_abx_window,
  delay_window_medical,
  first_abx_delay_hours_clean,
  artificial_censor_h,
  artificial_censor_reason,
  strategy_adherent_by_delay,
  last_weight_hour,
  weight_reached_arm_end,
  natural_censor_before_arm_end,
  empty_ipcw_product,
  death_30d,
  fu_time_30d_days,
  final_ipcw,
  final_ipcw_trunc_p01_p99,
  final_ipcw_trunc_p05_p95
)]

model_diag <- rbindlist(
  list(
    cbind(section = "model_counts", model_counts),
    cbind(section = "event_by_hour", event_by_hour),
    cbind(section = "weight_missing", weight_missing_diag),
    cbind(section = "boundary_check", boundary_check),
    cbind(section = "fit_log", fit_log)
  ),
  fill = TRUE
)

fwrite(weights_out, out_weights)
fwrite(diag_all, out_diag)
fwrite(model_diag, out_model_diag)

if (write_time_weight_output) {
  time_weights_out <- model_dat[, .(
    .imp,
    clone_id,
    stay_id,
    arm,
    arm_id,
    t_hour,
    artificial_censor_event,
    p_num_censor,
    p_den_censor,
    surv_ratio,
    ipcw_t
  )]
  fwrite(time_weights_out, out_time_weights)
}

print_section("5. Output")
cat("Wrote weights:", out_weights, "\n")
cat("Rows:", nrow(weights_out), "\n")
cat("Wrote diagnostics:", out_diag, "\n")
cat("Wrote model diagnostics:", out_model_diag, "\n")
if (write_time_weight_output) {
  cat("Wrote time-varying weights:", out_time_weights, "\n")
} else {
  cat("Time-varying weight table not written by default.\n")
}
cat("PASS: IPCW final weights estimated with rows through censor event.\n")
