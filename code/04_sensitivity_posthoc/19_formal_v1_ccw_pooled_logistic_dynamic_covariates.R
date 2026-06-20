# formal_v1 CCW weighted pooled logistic model with dynamic covariate set
# Scope: post-hoc subgroup/sensitivity analyses where some main-model
# covariates can become constant within the analysis subset.

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
analysis_label <- if (length(args) >= 2) args[2] else basename(base_dir)

pp_file <- file.path(base_dir, "formal_v1_ccw_30d_person_period_m5.csv")

out_coef <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_main_coef_mi.csv")
out_contrast <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_main_contrasts_mi.csv")
out_risk <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_main_risk_mi.csv")
out_diag <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_main_diagnostics.csv")
out_report <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_main_report.txt")

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

cluster_vcov_glm <- function(fit, data, cluster) {
  x <- model.matrix(fit)
  beta <- coef(fit)
  keep <- !is.na(beta)
  x <- x[, keep, drop = FALSE]

  y <- model.response(model.frame(fit))
  mu <- as.numeric(fitted(fit))
  w <- as.numeric(weights(fit, type = "prior"))

  info_w <- w * mu * (1 - mu)
  info <- crossprod(x, x * info_w)
  bread <- safe_solve(info)

  score_scalar <- w * (y - mu)
  score_mat <- x * score_scalar
  cluster_scores <- rowsum(score_mat, group = cluster, reorder = FALSE)
  meat <- crossprod(cluster_scores)

  g <- nrow(cluster_scores)
  small_sample <- if (g > 1L) g / (g - 1L) else 1
  vc <- small_sample * bread %*% meat %*% bread
  dimnames(vc) <- list(names(beta)[keep], names(beta)[keep])
  vc
}

pool_scalar <- function(q, u, label, transform = FALSE) {
  q <- as.numeric(q)
  u <- as.numeric(u)
  ok <- is.finite(q) & is.finite(u) & u >= 0
  q <- q[ok]
  u <- u[ok]
  m <- length(q)
  if (m == 0L) {
    return(data.table(
      term = label,
      m = 0L,
      estimate = NA_real_,
      robust_se = NA_real_,
      statistic = NA_real_,
      df = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      or = NA_real_,
      or_ci_low = NA_real_,
      or_ci_high = NA_real_
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
      haz <- pmin(pmax(haz, 0), 1)
      surv <- surv * (1 - haz)
    }
    out[[aa]] <- data.table(
      arm = aa,
      cumulative_risk_30d = mean(1 - surv),
      survival_30d = mean(surv)
    )
  }

  rbindlist(out)
}

has_variation <- function(x) {
  ux <- unique(x[!is.na(x)])
  length(ux) > 1L
}

if (!file.exists(pp_file)) stop("Person-period file not found: ", pp_file, call. = FALSE)

cat("formal_v1 CCW dynamic-covariate pooled logistic model\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Analysis label:", analysis_label, "\n")
cat("Person-period table:", pp_file, "\n")

need_cols <- c(
  ".imp", "clone_id", "stay_id", "arm", "time_axis", "event_30d_period",
  "ipcw_stabilized",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing", "shock_t0_b_num_model", "sofa_total_t0_imp"
)
pp_header <- names(fread(pp_file, nrows = 0, showProgress = FALSE))
missing_cols <- setdiff(need_cols, pp_header)
if (length(missing_cols) > 0) {
  stop("Person-period table missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

pp <- fread(pp_file, select = need_cols, showProgress = FALSE)
pp[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
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

candidate_terms <- c(
  "age_at_icu_model",
  "sex_factor",
  "race_group",
  "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_imp",
  "sofa_total_t0_missing",
  "shock_t0_b_num_model"
)

term_variation <- data.table(
  term = candidate_terms,
  included = vapply(candidate_terms, function(v) has_variation(pp[[v]]), logical(1))
)
covar_terms <- term_variation[included == TRUE, term]

model_cols <- c("event_30d_period", "ipcw_stabilized", "arm", "time_axis", covar_terms)
model_na <- pp[, lapply(.SD, function(x) sum(is.na(x))), by = .imp, .SDcols = model_cols]
if (model_na[, any(rowSums(.SD) > 0), .SDcols = model_cols]) {
  print(model_na)
  stop("Outcome model columns contain NA.", call. = FALSE)
}
if (pp[, any(!is.finite(ipcw_stabilized) | ipcw_stabilized <= 0)]) {
  stop("Non-positive or non-finite IPCW detected.", call. = FALSE)
}

main_formula <- as.formula(paste(
  "event_30d_period ~ arm + time_axis",
  if (length(covar_terms) > 0L) paste("+", paste(covar_terms, collapse = " + ")) else ""
))

print_section("1. Dynamic Covariate Set")
print(term_variation)
cat("Formula:", paste(deparse(main_formula, width.cutoff = 500L), collapse = " "), "\n")

print_section("2. Input Diagnostics")
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
risk_long <- list()
diag_fit <- list()

for (ii in sort(unique(pp$.imp))) {
  fit_dat <- pp[.imp == ii]
  fit_dat[, stay_cluster := as.character(stay_id)]

  cat("Fitting .imp =", ii, "rows =", nrow(fit_dat), "\n")
  fit <- suppressWarnings(glm(
    main_formula,
    data = fit_dat,
    family = binomial(),
    weights = ipcw_stabilized
  ))
  vc <- cluster_vcov_glm(fit, fit_dat, fit_dat$stay_cluster)
  beta_all <- coef(fit)
  beta <- beta_all[!is.na(beta_all)]
  var_diag <- as.numeric(diag(vc)[names(beta)])
  se <- sqrt(ifelse(var_diag >= 0, var_diag, NA_real_))
  negative_variance_terms <- names(beta)[!is.na(var_diag) & var_diag < 0]
  strategy_variance_ok <- all(
    c("arm3-6h", "arm6-12h") %in% names(beta),
    is.finite(var_diag[match(c("arm3-6h", "arm6-12h"), names(beta))]),
    var_diag[match(c("arm3-6h", "arm6-12h"), names(beta))] > 0
  )

  coef_long[[as.character(ii)]] <- data.table(
    .imp = ii,
    term = names(beta),
    estimate = as.numeric(beta),
    robust_se = as.numeric(se),
    variance = var_diag,
    or = exp(as.numeric(beta)),
    ci_low = exp(as.numeric(beta) - 1.96 * as.numeric(se)),
    ci_high = exp(as.numeric(beta) + 1.96 * as.numeric(se))
  )

  if (all(c("arm3-6h", "arm6-12h") %in% names(beta))) {
    l_36 <- rep(0, length(beta))
    names(l_36) <- names(beta)
    l_36["arm3-6h"] <- 1

    l_612 <- rep(0, length(beta))
    names(l_612) <- names(beta)
    l_612["arm6-12h"] <- 1

    l_612_vs_36 <- l_612 - l_36

    contrast_specs <- list(
      "3-6h_vs_0-3h" = l_36,
      "6-12h_vs_0-3h" = l_612,
      "6-12h_vs_3-6h" = l_612_vs_36
    )
    contrast_long[[as.character(ii)]] <- rbindlist(lapply(names(contrast_specs), function(lbl) {
      l <- contrast_specs[[lbl]]
      est <- sum(l * beta)
      var <- as.numeric(t(l) %*% vc[names(beta), names(beta), drop = FALSE] %*% l)
      data.table(
        .imp = ii,
        contrast = lbl,
        estimate = est,
        variance = var,
        robust_se = sqrt(var),
        or = exp(est),
        ci_low = exp(est - 1.96 * sqrt(var)),
        ci_high = exp(est + 1.96 * sqrt(var))
      )
    }))
  }

  base_cols <- unique(c("stay_id", covar_terms))
  base_stay <- unique(fit_dat[, ..base_cols], by = "stay_id")
  if ("sex_factor" %in% names(base_stay)) base_stay[, sex_factor := factor(sex_factor, levels = sex_levels)]
  if ("race_group" %in% names(base_stay)) base_stay[, race_group := factor(race_group, levels = race_levels)]
  if ("admission_type_group" %in% names(base_stay)) base_stay[, admission_type_group := factor(admission_type_group, levels = admission_levels)]

  risk_i <- standardized_risk(fit, base_stay, arm_levels, time_levels)
  risk_i[, .imp := ii]
  ref <- risk_i[arm == "0-3h", cumulative_risk_30d]
  risk_i[, risk_difference_vs_0_3h := cumulative_risk_30d - ref]
  risk_i[, risk_ratio_vs_0_3h := cumulative_risk_30d / ref]
  risk_long[[as.character(ii)]] <- risk_i

  diag_fit[[as.character(ii)]] <- data.table(
    .imp = ii,
    rows = nrow(fit_dat),
    clusters = uniqueN(fit_dat$stay_id),
    clones = uniqueN(fit_dat$clone_id),
    events = sum(fit_dat$event_30d_period),
    weighted_events = sum(fit_dat$event_30d_period * fit_dat$ipcw_stabilized),
    coefficients = length(beta),
    aliased_coefficients = sum(is.na(beta_all)),
    negative_variance_terms = length(negative_variance_terms),
    negative_variance_term_names = paste(negative_variance_terms, collapse = ";"),
    strategy_variance_ok = strategy_variance_ok,
    converged = isTRUE(fit$converged),
    aic = fit$aic
  )
}

coef_long <- rbindlist(coef_long, fill = TRUE)
contrast_long <- rbindlist(contrast_long, fill = TRUE)
risk_long <- rbindlist(risk_long, fill = TRUE)
diag_fit <- rbindlist(diag_fit, fill = TRUE)

coef_pooled <- rbindlist(lapply(unique(coef_long$term), function(tt) {
  x <- coef_long[term == tt]
  pool_scalar(x$estimate, x$variance, tt, transform = TRUE)
}), fill = TRUE)
setorder(coef_pooled, term)

contrast_pooled <- rbindlist(lapply(unique(contrast_long$contrast), function(cc) {
  x <- contrast_long[contrast == cc]
  out <- pool_scalar(x$estimate, x$variance, cc, transform = TRUE)
  setnames(out, "term", "contrast")
  out
}), fill = TRUE)

risk_summary <- risk_long[, .(
  m = .N,
  cumulative_risk_30d = mean(cumulative_risk_30d),
  risk_min = min(cumulative_risk_30d),
  risk_max = max(cumulative_risk_30d),
  risk_difference_vs_0_3h = mean(risk_difference_vs_0_3h),
  risk_ratio_vs_0_3h = mean(risk_ratio_vs_0_3h)
), by = arm][order(factor(arm, levels = arm_levels))]

print_section("3. Fit Diagnostics")
print(diag_fit)

print_section("4. Pooled Strategy Contrasts")
print(contrast_pooled)

print_section("5. Standardized 30-day Risks")
print(risk_summary)

coef_out <- rbindlist(list(
  cbind(section = "pooled", coef_pooled),
  cbind(section = "per_imp", coef_long)
), fill = TRUE)
contrast_out <- rbindlist(list(
  cbind(section = "pooled", contrast_pooled),
  cbind(section = "per_imp", contrast_long)
), fill = TRUE)
risk_out <- rbindlist(list(
  cbind(section = "summary", risk_summary),
  cbind(section = "per_imp", risk_long)
), fill = TRUE)
diag_out <- rbindlist(list(
  cbind(section = "dynamic_terms", term_variation),
  cbind(section = "input_by_arm", input_diag),
  cbind(section = "fit_by_imp", diag_fit)
), fill = TRUE)

fwrite(coef_out, out_coef)
fwrite(contrast_out, out_contrast)
fwrite(risk_out, out_risk)
fwrite(diag_out, out_diag)

cat("=== formal_v1 CCW dynamic-covariate weighted pooled logistic model ===\n", file = out_report)
cat("analysis_label:", analysis_label, "\n", file = out_report, append = TRUE)
cat("input:", pp_file, "\n", file = out_report, append = TRUE)
cat("model: ", paste(deparse(main_formula, width.cutoff = 500L), collapse = " "), "\n", file = out_report, append = TRUE)
cat("weights: ipcw_stabilized, untruncated\n", file = out_report, append = TRUE)
cat("variance: sandwich robust clustered by original stay_id, pooled by Rubin rules\n", file = out_report, append = TRUE)
cat("constant covariates were omitted from the outcome model by design for post-hoc subgroup analyses.\n\n", file = out_report, append = TRUE)

cat("dynamic_covariate_set:\n", file = out_report, append = TRUE)
capture.output(print(term_variation), file = out_report, append = TRUE)
cat("\nfit_diagnostics:\n", file = out_report, append = TRUE)
capture.output(print(diag_fit), file = out_report, append = TRUE)
cat("\npooled_strategy_contrasts:\n", file = out_report, append = TRUE)
capture.output(print(contrast_pooled), file = out_report, append = TRUE)
cat("\nstandardized_30d_risks:\n", file = out_report, append = TRUE)
capture.output(print(risk_summary), file = out_report, append = TRUE)
cat("\nnotes:\n", file = out_report, append = TRUE)
cat("Odds ratios are discrete-time pooled-logistic odds ratios.\n", file = out_report, append = TRUE)
cat("Risks are model-standardized over the post-hoc analysis subset covariate distribution.\n", file = out_report, append = TRUE)
cat("Risk uncertainty is not computed in this script; point risks are averaged across imputations.\n", file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

print_section("6. Output")
cat("Wrote pooled coefficients:", out_coef, "\n")
cat("Wrote pooled contrasts:", out_contrast, "\n")
cat("Wrote standardized risks:", out_risk, "\n")
cat("Wrote diagnostics:", out_diag, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: dynamic-covariate pooled logistic model completed.\n")
