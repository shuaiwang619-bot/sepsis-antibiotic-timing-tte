# formal_v1 CCW pooled logistic sensitivity with truncated IPCW
# Scope: refit the main full-time-axis pooled logistic model with p01-p99 and
# p05-p95 truncated weights.

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
out_contrast <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_truncated_weights_contrasts_mi.csv")
out_risk <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_truncated_weights_risk_mi.csv")
out_diag <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_truncated_weights_diagnostics.csv")
out_report <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_truncated_weights_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

safe_solve <- function(x) {
  tryCatch(solve(x), error = function(e) qr.solve(x))
}

cluster_vcov_glm <- function(fit, data, cluster, weight_col) {
  x <- model.matrix(fit)
  y <- model.response(model.frame(fit))
  mu <- as.numeric(fitted(fit))
  w <- as.numeric(data[[weight_col]])
  bread <- safe_solve(crossprod(x, x * (w * mu * (1 - mu))))
  score_mat <- x * (w * (y - mu))
  cluster_scores <- rowsum(score_mat, group = cluster, reorder = FALSE)
  meat <- crossprod(cluster_scores)
  g <- nrow(cluster_scores)
  correction <- if (g > 1L) g / (g - 1L) else 1
  correction * bread %*% meat %*% bread
}

pool_scalar <- function(q, u, label, transform = FALSE) {
  ok <- is.finite(q) & is.finite(u) & u >= 0
  q <- as.numeric(q)[ok]
  u <- as.numeric(u)[ok]
  m <- length(q)
  if (m == 0L) {
    return(data.table(
      contrast = label, m = 0L, estimate = NA_real_, robust_se = NA_real_,
      statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_,
      or = NA_real_, or_ci_low = NA_real_, or_ci_high = NA_real_
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
    contrast = label,
    m = m,
    estimate = qbar,
    robust_se = se,
    statistic = stat,
    df = df,
    p_value = p,
    conf_low = qbar - crit * se,
    conf_high = qbar + crit * se,
    or = if (transform) exp(qbar) else NA_real_,
    or_ci_low = if (transform) exp(qbar - crit * se) else NA_real_,
    or_ci_high = if (transform) exp(qbar + crit * se) else NA_real_
  )
}

standardized_risk <- function(fit, base_stay, arm_levels, time_levels) {
  out <- vector("list", length(arm_levels))
  names(out) <- arm_levels
  for (aa in arm_levels) {
    surv <- rep(1, nrow(base_stay))
    for (tt in time_levels) {
      nd <- copy(base_stay)
      nd[, arm := factor(aa, levels = arm_levels)]
      nd[, time_axis := factor(tt, levels = time_levels)]
      haz <- as.numeric(predict(fit, newdata = nd, type = "response"))
      surv <- surv * (1 - pmin(pmax(haz, 0), 1))
    }
    out[[aa]] <- data.table(
      arm = aa,
      cumulative_risk_30d = mean(1 - surv),
      survival_30d = mean(surv)
    )
  }
  rbindlist(out)
}

fit_one_weight <- function(pp, weight_col, weight_label, arm_levels, time_levels,
                           sex_levels, race_levels, admission_levels, formula_obj) {
  contrast_long <- list()
  risk_long <- list()
  diag_fit <- list()

  for (ii in sort(unique(pp$.imp))) {
    fit_dat <- pp[.imp == ii]
    fit_dat[, model_weight := as_num(get(weight_col))]
    if (fit_dat[, any(!is.finite(model_weight) | model_weight <= 0)]) {
      stop("Bad weights in ", weight_label, " .imp=", ii, call. = FALSE)
    }
    cat("Fitting", weight_label, ".imp =", ii, "rows =", nrow(fit_dat), "\n")
    fit <- suppressWarnings(glm(
      formula_obj,
      data = fit_dat,
      family = binomial(),
      weights = model_weight
    ))
    vc <- cluster_vcov_glm(fit, fit_dat, as.character(fit_dat$stay_id), "model_weight")
    beta <- coef(fit)
    var_diag <- as.numeric(diag(vc))
    negative_variance_terms <- names(beta)[!is.na(var_diag) & var_diag < 0]

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
    contrast_long[[paste(weight_label, ii, sep = "_")]] <- rbindlist(lapply(names(contrast_specs), function(lbl) {
      l <- contrast_specs[[lbl]]
      est <- sum(l * beta)
      var <- as.numeric(t(l) %*% vc %*% l)
      data.table(
        weight_scheme = weight_label,
        .imp = ii,
        contrast = lbl,
        estimate = est,
        variance = var,
        robust_se = sqrt(ifelse(var >= 0, var, NA_real_)),
        or = exp(est)
      )
    }))

    base_stay <- unique(fit_dat[, .(
      stay_id,
      age_at_icu_model,
      sex_factor,
      race_group,
      admission_type_group,
      charlson_comorbidity_index_model,
      sofa_total_t0_imp,
      sofa_total_t0_missing,
      shock_t0_b_num_model
    )], by = "stay_id")
    base_stay[, sex_factor := factor(sex_factor, levels = sex_levels)]
    base_stay[, race_group := factor(race_group, levels = race_levels)]
    base_stay[, admission_type_group := factor(admission_type_group, levels = admission_levels)]

    risk_i <- standardized_risk(fit, base_stay, arm_levels, time_levels)
    risk_i[, `:=`(weight_scheme = weight_label, .imp = ii)]
    ref <- risk_i[arm == "0-3h", cumulative_risk_30d]
    risk_i[, risk_difference_vs_0_3h := cumulative_risk_30d - ref]
    risk_i[, risk_ratio_vs_0_3h := cumulative_risk_30d / ref]
    risk_long[[paste(weight_label, ii, sep = "_")]] <- risk_i

    diag_fit[[paste(weight_label, ii, sep = "_")]] <- data.table(
      weight_scheme = weight_label,
      .imp = ii,
      rows = nrow(fit_dat),
      clusters = uniqueN(fit_dat$stay_id),
      clones = uniqueN(fit_dat$clone_id),
      events = sum(fit_dat$event_30d_period),
      weighted_events = sum(fit_dat$event_30d_period * fit_dat$model_weight),
      mean_weight = mean(fit_dat$model_weight),
      max_weight = max(fit_dat$model_weight),
      coefficients = length(beta),
      negative_variance_terms = length(negative_variance_terms),
      negative_variance_term_names = paste(negative_variance_terms, collapse = ";"),
      strategy_variance_ok = all(
        c("arm3-6h", "arm6-12h") %in% names(beta),
        is.finite(var_diag[match(c("arm3-6h", "arm6-12h"), names(beta))]),
        var_diag[match(c("arm3-6h", "arm6-12h"), names(beta))] > 0
      ),
      converged = isTRUE(fit$converged),
      aic = fit$aic
    )
  }

  contrast_long <- rbindlist(contrast_long, fill = TRUE)
  risk_long <- rbindlist(risk_long, fill = TRUE)
  diag_fit <- rbindlist(diag_fit, fill = TRUE)

  contrast_pooled <- rbindlist(lapply(unique(contrast_long$contrast), function(cc) {
    x <- contrast_long[contrast == cc]
    out <- pool_scalar(x$estimate, x$variance, cc, transform = TRUE)
    out[, weight_scheme := weight_label]
    out
  }), fill = TRUE)

  risk_summary <- risk_long[, .(
    m = .N,
    cumulative_risk_30d = mean(cumulative_risk_30d),
    risk_min = min(cumulative_risk_30d),
    risk_max = max(cumulative_risk_30d),
    risk_difference_vs_0_3h = mean(risk_difference_vs_0_3h),
    risk_ratio_vs_0_3h = mean(risk_ratio_vs_0_3h)
  ), by = .(weight_scheme, arm)]

  list(
    contrast_pooled = contrast_pooled,
    contrast_long = contrast_long,
    risk_summary = risk_summary,
    risk_long = risk_long,
    diag_fit = diag_fit
  )
}

if (!file.exists(pp_file)) stop("Person-period file not found: ", pp_file, call. = FALSE)

cat("formal_v1 CCW truncated-weight pooled logistic sensitivity\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Person-period table:", pp_file, "\n")

need_cols <- c(
  ".imp", "clone_id", "stay_id", "arm", "time_axis", "event_30d_period",
  "ipcw_stabilized_trunc_p01_p99", "ipcw_stabilized_trunc_p05_p95",
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
  event_30d_period = as.integer(as_num(event_30d_period)),
  ipcw_stabilized_trunc_p01_p99 = as_num(ipcw_stabilized_trunc_p01_p99),
  ipcw_stabilized_trunc_p05_p95 = as_num(ipcw_stabilized_trunc_p05_p95),
  age_at_icu_model = as_num(age_at_icu_model),
  charlson_comorbidity_index_model = as_num(charlson_comorbidity_index_model),
  sofa_total_t0_missing = as.integer(as_num(sofa_total_t0_missing)),
  shock_t0_b_num_model = as.integer(as_num(shock_t0_b_num_model)),
  sofa_total_t0_imp = as_num(sofa_total_t0_imp)
)]
pp <- pp[.imp %in% 1:5]

arm_levels <- c("0-3h", "3-6h", "6-12h")
time_levels <- c(paste0("h", 0:23), paste0("day", 2:30))
sex_levels <- sort(unique(as.character(pp$sex_factor)))
race_levels <- c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER", "UNKNOWN")
race_levels <- c(race_levels[race_levels %in% unique(pp$race_group)], setdiff(sort(unique(pp$race_group)), race_levels))
admission_levels <- c("EMERGENCY", "URGENT", "ELECTIVE_SURGICAL", "OBSERVATION")
admission_levels <- c(admission_levels[admission_levels %in% unique(pp$admission_type_group)], setdiff(sort(unique(pp$admission_type_group)), admission_levels))

pp[, arm := factor(arm, levels = arm_levels)]
pp[, time_axis := factor(time_axis, levels = time_levels)]
pp[, sex_factor := factor(sex_factor, levels = sex_levels)]
pp[, race_group := factor(race_group, levels = race_levels)]
pp[, admission_type_group := factor(admission_type_group, levels = admission_levels)]

model_cols <- c(
  "event_30d_period", "arm", "time_axis",
  "ipcw_stabilized_trunc_p01_p99", "ipcw_stabilized_trunc_p05_p95",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model", "sofa_total_t0_imp",
  "sofa_total_t0_missing", "shock_t0_b_num_model"
)
model_na <- pp[, lapply(.SD, function(x) sum(is.na(x))), by = .imp, .SDcols = model_cols]
if (model_na[, any(rowSums(.SD) > 0), .SDcols = model_cols]) {
  print(model_na)
  stop("Truncated-weight model columns contain NA.", call. = FALSE)
}

main_formula <- event_30d_period ~
  arm +
  time_axis +
  age_at_icu_model +
  sex_factor +
  race_group +
  admission_type_group +
  charlson_comorbidity_index_model +
  sofa_total_t0_imp +
  sofa_total_t0_missing +
  shock_t0_b_num_model

schemes <- list(
  p01_p99 = "ipcw_stabilized_trunc_p01_p99",
  p05_p95 = "ipcw_stabilized_trunc_p05_p95"
)

results <- lapply(names(schemes), function(lbl) {
  fit_one_weight(
    pp = pp,
    weight_col = schemes[[lbl]],
    weight_label = lbl,
    arm_levels = arm_levels,
    time_levels = time_levels,
    sex_levels = sex_levels,
    race_levels = race_levels,
    admission_levels = admission_levels,
    formula_obj = main_formula
  )
})
names(results) <- names(schemes)

contrast_pooled <- rbindlist(lapply(results, `[[`, "contrast_pooled"), fill = TRUE)
contrast_long <- rbindlist(lapply(results, `[[`, "contrast_long"), fill = TRUE)
risk_summary <- rbindlist(lapply(results, `[[`, "risk_summary"), fill = TRUE)
risk_long <- rbindlist(lapply(results, `[[`, "risk_long"), fill = TRUE)
diag_fit <- rbindlist(lapply(results, `[[`, "diag_fit"), fill = TRUE)

setcolorder(contrast_pooled, c("weight_scheme", setdiff(names(contrast_pooled), "weight_scheme")))
setcolorder(risk_summary, c("weight_scheme", setdiff(names(risk_summary), "weight_scheme")))
setorder(contrast_pooled, weight_scheme, contrast)
risk_summary[, arm_order := match(arm, arm_levels)]
setorder(risk_summary, weight_scheme, arm_order)
risk_summary[, arm_order := NULL]

print_section("1. Fit Diagnostics")
print(diag_fit)
print_section("2. Pooled Truncated-Weight Contrasts")
print(contrast_pooled)
print_section("3. Standardized 30-day Risks")
print(risk_summary)

contrast_out <- rbindlist(list(
  cbind(section = "pooled", contrast_pooled),
  cbind(section = "per_imp", contrast_long)
), fill = TRUE)
risk_out <- rbindlist(list(
  cbind(section = "summary", risk_summary),
  cbind(section = "per_imp", risk_long)
), fill = TRUE)

fwrite(contrast_out, out_contrast)
fwrite(risk_out, out_risk)
fwrite(diag_fit, out_diag)

cat("=== formal_v1 CCW pooled logistic truncated-weight sensitivity ===\n", file = out_report)
cat("input:", pp_file, "\n", file = out_report, append = TRUE)
cat("model:", paste(deparse(main_formula, width.cutoff = 500L), collapse = " "), "\n", file = out_report, append = TRUE)
cat("schemes: p01_p99, p05_p95\n\n", file = out_report, append = TRUE)
cat("fit_diagnostics:\n", file = out_report, append = TRUE)
capture.output(print(diag_fit), file = out_report, append = TRUE)
cat("\npooled_truncated_weight_contrasts:\n", file = out_report, append = TRUE)
capture.output(print(contrast_pooled), file = out_report, append = TRUE)
cat("\nstandardized_30d_risks:\n", file = out_report, append = TRUE)
capture.output(print(risk_summary), file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

print_section("4. Output")
cat("Wrote contrasts:", out_contrast, "\n")
cat("Wrote risks:", out_risk, "\n")
cat("Wrote diagnostics:", out_diag, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: truncated-weight pooled logistic sensitivity completed.\n")
