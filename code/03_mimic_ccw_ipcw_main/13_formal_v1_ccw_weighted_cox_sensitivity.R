# formal_v1 CCW weighted Cox sensitivity model
# Scope: counting-process Cox model on the 30-day person-period table,
# fit separately in .imp = 1-5 and combined with Rubin rules.

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

pp_file <- file.path(base_dir, "formal_v1_ccw_30d_person_period_m5.csv")
out_coef <- file.path(base_dir, "formal_v1_ccw_30d_weighted_cox_sensitivity_coef_mi.csv")
out_contrast <- file.path(base_dir, "formal_v1_ccw_30d_weighted_cox_sensitivity_contrasts_mi.csv")
out_diag <- file.path(base_dir, "formal_v1_ccw_30d_weighted_cox_sensitivity_diagnostics.csv")
out_report <- file.path(base_dir, "formal_v1_ccw_30d_weighted_cox_sensitivity_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
if (!requireNamespace("survival", quietly = TRUE)) {
  stop("Package 'survival' is required.", call. = FALSE)
}

library(data.table)

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

pool_scalar <- function(q, u, label, transform = FALSE) {
  ok <- is.finite(q) & is.finite(u) & u >= 0
  q <- as.numeric(q)[ok]
  u <- as.numeric(u)[ok]
  m <- length(q)
  if (m == 0L) {
    return(data.table(
      term = label, m = 0L, estimate = NA_real_, robust_se = NA_real_,
      statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_,
      hr = NA_real_, hr_ci_low = NA_real_, hr_ci_high = NA_real_
    ))
  }
  qbar <- mean(q)
  ubar <- mean(u)
  b <- if (m > 1L) stats::var(q) else 0
  total <- ubar + (1 + 1 / m) * b
  se <- sqrt(total)
  df <- if (m > 1L && b > 0 && is.finite(ubar)) {
    (m - 1) * (1 + ubar / ((1 + 1 / m) * b))^2
  } else {
    Inf
  }
  crit <- if (is.finite(df)) stats::qt(0.975, df = df) else stats::qnorm(0.975)
  stat <- qbar / se
  p <- if (is.finite(df)) {
    2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
  } else {
    2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  }
  data.table(
    term = label,
    m = m,
    estimate = qbar,
    robust_se = se,
    statistic = stat,
    df = df,
    p_value = p,
    conf_low = qbar - crit * se,
    conf_high = qbar + crit * se,
    hr = if (transform) exp(qbar) else NA_real_,
    hr_ci_low = if (transform) exp(qbar - crit * se) else NA_real_,
    hr_ci_high = if (transform) exp(qbar + crit * se) else NA_real_
  )
}

if (!file.exists(pp_file)) stop("Person-period file not found: ", pp_file, call. = FALSE)

cat("formal_v1 CCW weighted Cox sensitivity model\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Person-period table:", pp_file, "\n")

need_cols <- c(
  ".imp", "clone_id", "stay_id", "arm", "time_start_h", "time_end_h",
  "event_30d_period", "ipcw_stabilized",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing", "shock_t0_b_num_model", "sofa_total_t0_imp"
)
header <- names(fread(pp_file, nrows = 0, showProgress = FALSE))
missing_cols <- setdiff(need_cols, header)
if (length(missing_cols) > 0) stop("Person-period missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)

pp <- fread(pp_file, select = need_cols, showProgress = FALSE)
pp[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  time_start_h = as_num(time_start_h),
  time_end_h = as_num(time_end_h),
  event_30d_period = as.integer(as_num(event_30d_period)),
  ipcw_stabilized = as_num(ipcw_stabilized),
  age_at_icu_model = as_num(age_at_icu_model),
  charlson_comorbidity_index_model = as_num(charlson_comorbidity_index_model),
  sofa_total_t0_missing = as.integer(as_num(sofa_total_t0_missing)),
  shock_t0_b_num_model = as.integer(as_num(shock_t0_b_num_model)),
  sofa_total_t0_imp = as_num(sofa_total_t0_imp)
)]
pp <- pp[.imp %in% 1:5]

arm_levels <- c("0-3h", "3-6h", "6-12h")
sex_levels <- sort(unique(as.character(pp$sex_factor)))
race_levels <- c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER", "UNKNOWN")
race_levels <- c(race_levels[race_levels %in% unique(pp$race_group)], setdiff(sort(unique(pp$race_group)), race_levels))
admission_levels <- c("EMERGENCY", "URGENT", "ELECTIVE_SURGICAL", "OBSERVATION")
admission_levels <- c(admission_levels[admission_levels %in% unique(pp$admission_type_group)], setdiff(sort(unique(pp$admission_type_group)), admission_levels))

pp[, arm := factor(arm, levels = arm_levels)]
pp[, sex_factor := factor(sex_factor, levels = sex_levels)]
pp[, race_group := factor(race_group, levels = race_levels)]
pp[, admission_type_group := factor(admission_type_group, levels = admission_levels)]
pp <- pp[time_end_h > time_start_h]

model_cols <- c(
  "time_start_h", "time_end_h", "event_30d_period", "ipcw_stabilized",
  "arm", "age_at_icu_model", "sex_factor", "race_group",
  "admission_type_group", "charlson_comorbidity_index_model",
  "sofa_total_t0_imp", "sofa_total_t0_missing", "shock_t0_b_num_model"
)
model_na <- pp[, lapply(.SD, function(x) sum(is.na(x))), by = .imp, .SDcols = model_cols]
if (model_na[, any(rowSums(.SD) > 0), .SDcols = model_cols]) {
  print(model_na)
  stop("Cox model columns contain NA.", call. = FALSE)
}
if (pp[, any(!is.finite(ipcw_stabilized) | ipcw_stabilized <= 0)]) {
  stop("Non-positive or non-finite IPCW detected.", call. = FALSE)
}

cox_formula <- survival::Surv(time_start_h, time_end_h, event_30d_period) ~
  arm +
  age_at_icu_model +
  sex_factor +
  race_group +
  admission_type_group +
  charlson_comorbidity_index_model +
  sofa_total_t0_imp +
  sofa_total_t0_missing +
  shock_t0_b_num_model +
  cluster(stay_id)

print_section("1. Input Diagnostics")
input_diag <- pp[, .(
  rows = .N,
  clones = uniqueN(clone_id),
  stays = uniqueN(stay_id),
  events = sum(event_30d_period),
  weighted_events = sum(event_30d_period * ipcw_stabilized),
  mean_weight = mean(ipcw_stabilized),
  max_weight = max(ipcw_stabilized)
), by = .(.imp, arm)][order(.imp, arm)]
print(input_diag)

coef_long <- list()
contrast_long <- list()
diag_fit <- list()

for (ii in sort(unique(pp$.imp))) {
  fit_dat <- pp[.imp == ii]
  cat("Fitting .imp =", ii, "rows =", nrow(fit_dat), "\n")
  fit <- survival::coxph(
    cox_formula,
    data = fit_dat,
    weights = ipcw_stabilized,
    robust = TRUE,
    ties = "efron"
  )

  beta <- stats::coef(fit)
  vc <- fit$var
  var_diag <- as.numeric(diag(vc))
  se <- sqrt(ifelse(var_diag >= 0, var_diag, NA_real_))

  coef_long[[as.character(ii)]] <- data.table(
    .imp = ii,
    term = names(beta),
    estimate = as.numeric(beta),
    robust_se = se,
    variance = var_diag,
    hr = exp(as.numeric(beta))
  )

  l_36 <- rep(0, length(beta))
  names(l_36) <- names(beta)
  l_36["arm3-6h"] <- 1
  l_612 <- rep(0, length(beta))
  names(l_612) <- names(beta)
  l_612["arm6-12h"] <- 1
  contrast_specs <- list(
    "3-6h_vs_0-3h" = l_36,
    "6-12h_vs_0-3h" = l_612,
    "6-12h_vs_3-6h" = l_612 - l_36
  )
  contrast_long[[as.character(ii)]] <- rbindlist(lapply(names(contrast_specs), function(lbl) {
    l <- contrast_specs[[lbl]]
    est <- sum(l * beta)
    var <- as.numeric(t(l) %*% vc %*% l)
    data.table(
      .imp = ii,
      contrast = lbl,
      estimate = est,
      variance = var,
      robust_se = sqrt(ifelse(var >= 0, var, NA_real_)),
      hr = exp(est)
    )
  }))

  diag_fit[[as.character(ii)]] <- data.table(
    .imp = ii,
    rows = nrow(fit_dat),
    clusters = uniqueN(fit_dat$stay_id),
    clones = uniqueN(fit_dat$clone_id),
    events = sum(fit_dat$event_30d_period),
    weighted_events = sum(fit_dat$event_30d_period * fit_dat$ipcw_stabilized),
    coefficients = length(beta),
    negative_variance_terms = sum(var_diag < 0, na.rm = TRUE),
    fit_returned = TRUE,
    iterations = fit$iter,
    loglik = fit$loglik[2]
  )
}

coef_long <- rbindlist(coef_long, fill = TRUE)
contrast_long <- rbindlist(contrast_long, fill = TRUE)
diag_fit <- rbindlist(diag_fit, fill = TRUE)

coef_pooled <- rbindlist(lapply(unique(coef_long$term), function(tt) {
  x <- coef_long[term == tt]
  pool_scalar(x$estimate, x$variance, tt, transform = TRUE)
}), fill = TRUE)
contrast_pooled <- rbindlist(lapply(unique(contrast_long$contrast), function(cc) {
  x <- contrast_long[contrast == cc]
  out <- pool_scalar(x$estimate, x$variance, cc, transform = TRUE)
  setnames(out, "term", "contrast")
  out
}), fill = TRUE)

print_section("2. Fit Diagnostics")
print(diag_fit)
print_section("3. Pooled Cox Contrasts")
print(contrast_pooled)

coef_out <- rbindlist(list(
  cbind(section = "pooled", coef_pooled),
  cbind(section = "per_imp", coef_long)
), fill = TRUE)
contrast_out <- rbindlist(list(
  cbind(section = "pooled", contrast_pooled),
  cbind(section = "per_imp", contrast_long)
), fill = TRUE)
diag_out <- rbindlist(list(
  cbind(section = "input_by_arm", input_diag),
  cbind(section = "fit_by_imp", diag_fit)
), fill = TRUE)

fwrite(coef_out, out_coef)
fwrite(contrast_out, out_contrast)
fwrite(diag_out, out_diag)

cat("=== formal_v1 CCW weighted Cox sensitivity model ===\n", file = out_report)
cat("input:", pp_file, "\n", file = out_report, append = TRUE)
cat("model:", paste(deparse(cox_formula, width.cutoff = 500L), collapse = " "), "\n", file = out_report, append = TRUE)
cat("weights: ipcw_stabilized, untruncated\n", file = out_report, append = TRUE)
cat("variance: robust clustered by original stay_id via coxph cluster(stay_id), pooled by Rubin rules\n\n", file = out_report, append = TRUE)
cat("fit_diagnostics:\n", file = out_report, append = TRUE)
capture.output(print(diag_fit), file = out_report, append = TRUE)
cat("\npooled_cox_contrasts:\n", file = out_report, append = TRUE)
capture.output(print(contrast_pooled), file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

print_section("4. Output")
cat("Wrote coefficients:", out_coef, "\n")
cat("Wrote contrasts:", out_contrast, "\n")
cat("Wrote diagnostics:", out_diag, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: weighted Cox sensitivity completed.\n")
