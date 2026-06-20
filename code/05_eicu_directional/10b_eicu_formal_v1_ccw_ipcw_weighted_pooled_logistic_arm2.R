# eICU formal_v1 step 10b: arm2 CCW/IPCW weighted pooled logistic model
# MIMIC counterpart: 10_formal_v1_ccw_pooled_logistic_main.R
#
# Scope:
#   Supplemental immortal-time-aware analysis for the ICU-admission anchored
#   eICU arm2 comparison:
#     3-12h vs 0-3h antibiotic initiation.

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
pp_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_30d_person_period_m5.csv")
}
weight_col <- if (length(args) >= 3) args[3] else "ipcw_stabilized"

out_dir <- file.path(base_dir, "outputs", "ccw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_coef <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_30d_pooled_logistic_", weight_col, "_coef_mi.csv"))
out_contrast <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_30d_pooled_logistic_", weight_col, "_contrast_mi.csv"))
out_risk <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_30d_pooled_logistic_", weight_col, "_risk_mi.csv"))
out_diag <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_30d_pooled_logistic_", weight_col, "_diagnostics.csv"))
out_report <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_30d_pooled_logistic_", weight_col, "_report.txt"))

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

cluster_vcov_glm <- function(fit, data, cluster) {
  x <- stats::model.matrix(fit)
  y <- stats::model.response(stats::model.frame(fit))
  mu <- stats::fitted(fit)
  w <- as.numeric(stats::weights(fit, type = "prior"))
  if (is.null(w)) w <- rep(1, length(y))

  score_mat <- x * as.numeric((y - mu) * w)
  cluster <- as.factor(cluster)
  cluster_scores <- rowsum(score_mat, group = cluster, reorder = FALSE)
  meat <- crossprod(cluster_scores)
  bread <- tryCatch(stats::vcov(fit) * length(y), error = function(e) NULL)
  if (is.null(bread)) return(stats::vcov(fit))

  n <- nrow(x)
  g <- nrow(cluster_scores)
  k <- ncol(x)
  adj <- if (g > 1 && n > k) (g / (g - 1)) * ((n - 1) / (n - k)) else 1
  adj * bread %*% meat %*% bread / (n^2)
}

pool_scalar <- function(q, u, term, transform = TRUE) {
  q <- as.numeric(q)
  u <- as.numeric(u)
  ok <- is.finite(q) & is.finite(u) & !is.na(q) & !is.na(u) & u >= 0
  q <- q[ok]
  u <- u[ok]
  m <- length(q)
  if (m == 0L) {
    return(data.table(
      term = term, m = 0L, estimate = NA_real_, std_error = NA_real_,
      df = NA_real_, statistic = NA_real_, p_value = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_,
      or = NA_real_, or_low = NA_real_, or_high = NA_real_
    ))
  }
  qbar <- mean(q)
  ubar <- mean(u)
  bvar <- if (m > 1L) stats::var(q) else 0
  total_var <- ubar + (1 + 1 / m) * bvar
  se <- sqrt(total_var)
  df <- Inf
  if (m > 1L && bvar > 1e-12) {
    df <- (m - 1) * (1 + ubar / ((1 + 1 / m) * bvar))^2
  }
  crit <- if (is.finite(df)) stats::qt(0.975, df = df) else stats::qnorm(0.975)
  stat <- qbar / se
  p <- if (is.finite(df)) {
    2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
  } else {
    2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  }
  out <- data.table(
    term = term,
    m = m,
    estimate = qbar,
    std_error = se,
    df = df,
    statistic = stat,
    p_value = p,
    conf_low = qbar - crit * se,
    conf_high = qbar + crit * se
  )
  if (transform) {
    out[, `:=`(
      or = exp(estimate),
      or_low = exp(conf_low),
      or_high = exp(conf_high)
    )]
  }
  out
}

has_variation <- function(x) {
  if (is.factor(x)) return(nlevels(x) > 1L && length(unique(x[!is.na(x)])) > 1L)
  xx <- x[!is.na(x)]
  length(unique(xx)) > 1L
}

make_main_formula <- function(dat) {
  term_map <- list(
    "arm" = "arm",
    "time_axis" = "time_axis",
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
  keep <- union(c("arm", "time_axis"), keep)
  stats::as.formula(paste("event_30d_period ~", paste(keep, collapse = " + ")))
}

standardized_risk <- function(fit, base_stay, arm_levels, time_levels) {
  out <- vector("list", length(arm_levels))
  for (aa in seq_along(arm_levels)) {
    pred_parts <- vector("list", length(time_levels))
    for (tt in seq_along(time_levels)) {
      nd <- copy(base_stay)
      nd[, arm := factor(arm_levels[aa], levels = arm_levels)]
      nd[, time_axis := factor(time_levels[tt], levels = time_levels)]
      p <- stats::predict(fit, newdata = nd, type = "response")
      pred_parts[[tt]] <- data.table(patientunitstayid = nd$patientunitstayid, period = tt, p = p)
    }
    pred <- rbindlist(pred_parts)
    risk <- pred[, .(cumulative_risk_30d = 1 - prod(1 - p)), by = patientunitstayid]
    out[[aa]] <- data.table(
      arm = arm_levels[aa],
      cumulative_risk_30d = mean(risk$cumulative_risk_30d)
    )
  }
  rbindlist(out)
}

stop_if_missing(pp_file, "Step 09 person-period table")

pp <- fread(pp_file, showProgress = FALSE)

need <- c(
  ".imp", "clone_id", "patientunitstayid", "hospitalid",
  "arm", "time_axis", "event_30d_period", weight_col,
  "age_model", "apache_score_iva_model", "apache_score_iva_model_missing_ind",
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "diabetes_model_missing_ind"
)
missing_need <- setdiff(need, names(pp))
if (length(missing_need) > 0) stop("Person-period file missing columns: ", paste(missing_need, collapse = ", "), call. = FALSE)

pp[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  hospitalid = as.integer(to_num(hospitalid)),
  event_30d_period = as.integer(to_num(event_30d_period)),
  model_weight = to_num(get(weight_col)),
  age_model = to_num(age_model),
  apache_score_iva_model = to_num(apache_score_iva_model),
  apache_score_iva_model_missing_ind = as.integer(to_num(apache_score_iva_model_missing_ind)),
  hosp_zero_abx_reporting_ind = as.integer(to_num(hosp_zero_abx_reporting_ind)),
  diabetes_model = as.integer(to_num(diabetes_model)),
  diabetes_model_missing_ind = as.integer(to_num(diabetes_model_missing_ind))
)]

arm_levels <- c("0-3h", "3-12h")
time_levels <- c(paste0("h", 0:23), paste0("day", 2:30))
pp[, arm := factor(arm, levels = arm_levels)]
pp[, time_axis := factor(time_axis, levels = time_levels)]
pp[, gender_factor := factor(gender_factor)]
pp[, ethnicity_group := factor(ethnicity_group)]
pp[, unit_admit_source_group := factor(unit_admit_source_group)]
pp[, unit_type_group := factor(unit_type_group)]
pp[, hosp_window_volume_bucket_factor := factor(hosp_window_volume_bucket_factor)]
pp[, hosp_zero_abx_reporting_ind := factor(hosp_zero_abx_reporting_ind, levels = c(0, 1))]
pp[, diabetes_model := factor(diabetes_model, levels = c(0, 1))]
pp[, apache_score_iva_model_missing_ind := factor(apache_score_iva_model_missing_ind, levels = c(0, 1))]
pp[, diabetes_model_missing_ind := factor(diabetes_model_missing_ind, levels = c(0, 1))]

model_cols <- c(
  "event_30d_period", "model_weight", "arm", "time_axis",
  "age_model", "apache_score_iva_model", "apache_score_iva_model_missing_ind",
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "diabetes_model_missing_ind"
)
model_na <- pp[, lapply(.SD, function(x) sum(is.na(x))), by = .imp, .SDcols = model_cols]
if (model_na[, any(rowSums(.SD) > 0), .SDcols = model_cols]) {
  print(model_na)
  stop("Main CCW/IPCW model columns contain NA.", call. = FALSE)
}
if (pp[, any(!is.finite(model_weight) | model_weight <= 0)]) {
  stop("Non-positive or non-finite model weights detected.", call. = FALSE)
}

main_formula_template <- event_30d_period ~
  arm +
  time_axis +
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

input_diag <- pp[, .(
  rows = .N,
  clones = uniqueN(clone_id),
  stays = uniqueN(patientunitstayid),
  events = sum(event_30d_period),
  weighted_events = sum(event_30d_period * model_weight),
  mean_weight = mean(model_weight),
  max_weight = max(model_weight)
), by = .(.imp, arm)][order(.imp, arm)]

coef_long <- list()
contrast_long <- list()
risk_long <- list()
diag_fit <- list()

for (ii in sort(unique(pp$.imp))) {
  fit_dat <- copy(pp[.imp == ii])
  fit_dat[, stay_cluster := as.character(patientunitstayid)]
  main_formula <- make_main_formula(fit_dat)

  fit <- suppressWarnings(stats::glm(
    main_formula,
    data = fit_dat,
    family = stats::binomial(),
    weights = model_weight
  ))
  vc <- cluster_vcov_glm(fit, fit_dat, fit_dat$stay_cluster)
  beta <- stats::coef(fit)
  var_diag <- as.numeric(diag(vc))
  se <- sqrt(ifelse(var_diag >= 0, var_diag, NA_real_))

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

  arm_term <- setdiff(grep("^arm", names(beta), value = TRUE), "arm")
  if (length(arm_term) != 1L) {
    stop("Expected exactly one non-reference arm coefficient, found: ", paste(arm_term, collapse = ", "), call. = FALSE)
  }
  l_arm <- rep(0, length(beta))
  names(l_arm) <- names(beta)
  l_arm[arm_term] <- 1
  est <- sum(l_arm * beta)
  var <- as.numeric(t(l_arm) %*% vc %*% l_arm)
  contrast_long[[as.character(ii)]] <- data.table(
    .imp = ii,
    contrast = "3-12h_vs_0-3h",
    estimate = est,
    variance = var,
    robust_se = sqrt(var),
    or = exp(est),
    ci_low = exp(est - 1.96 * sqrt(var)),
    ci_high = exp(est + 1.96 * sqrt(var))
  )

  base_stay <- unique(fit_dat[, .(
    patientunitstayid,
    age_model,
    apache_score_iva_model,
    apache_score_iva_model_missing_ind,
    gender_factor,
    ethnicity_group,
    unit_admit_source_group,
    unit_type_group,
    hosp_window_volume_bucket_factor,
    hosp_zero_abx_reporting_ind,
    diabetes_model,
    diabetes_model_missing_ind
  )], by = "patientunitstayid")
  risk_i <- standardized_risk(fit, base_stay, arm_levels, time_levels)
  risk_i[, .imp := ii]
  ref <- risk_i[arm == "0-3h", cumulative_risk_30d]
  risk_i[, risk_difference_vs_0_3h := cumulative_risk_30d - ref]
  risk_i[, risk_ratio_vs_0_3h := cumulative_risk_30d / ref]
  risk_long[[as.character(ii)]] <- risk_i

  diag_fit[[as.character(ii)]] <- data.table(
    .imp = ii,
    rows = nrow(fit_dat),
    clusters = uniqueN(fit_dat$patientunitstayid),
    clones = uniqueN(fit_dat$clone_id),
    events = sum(fit_dat$event_30d_period),
    weighted_events = sum(fit_dat$event_30d_period * fit_dat$model_weight),
    coefficients = length(beta),
    converged = isTRUE(fit$converged),
    aic = fit$aic,
    arm_term = arm_term,
    arm_variance = var,
    formula = paste(deparse(main_formula, width.cutoff = 500L), collapse = " ")
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

coef_out <- rbindlist(list(cbind(section = "pooled", coef_pooled), cbind(section = "per_imp", coef_long)), fill = TRUE)
contrast_out <- rbindlist(list(cbind(section = "pooled", contrast_pooled), cbind(section = "per_imp", contrast_long)), fill = TRUE)
risk_out <- rbindlist(list(cbind(section = "summary", risk_summary), cbind(section = "per_imp", risk_long)), fill = TRUE)
diag_out <- rbindlist(list(cbind(section = "input_by_arm", input_diag), cbind(section = "fit_by_imp", diag_fit), cbind(section = "model_na", model_na)), fill = TRUE)

fwrite(coef_out, out_coef)
fwrite(contrast_out, out_contrast)
fwrite(risk_out, out_risk)
fwrite(diag_out, out_diag)

report_lines <- c(
  "eICU formal_v1 step 10b arm2 CCW/IPCW weighted pooled logistic model",
  paste("input:", pp_file),
  paste("weight_col:", weight_col),
  "",
  paste("model template:", paste(deparse(main_formula_template, width.cutoff = 500L), collapse = " ")),
  "Within each imputation, predictors with no variation are dropped before fitting.",
  "variance: sandwich robust clustered by original patientunitstayid, pooled by Rubin rules",
  "",
  "input diagnostics:",
  capture.output(print(input_diag)),
  "",
  "fit diagnostics:",
  capture.output(print(diag_fit)),
  "",
  "pooled contrast:",
  capture.output(print(contrast_pooled)),
  "",
  "standardized 30-day risks:",
  capture.output(print(risk_summary)),
  "",
  "notes:",
  "Odds ratios are discrete-time pooled-logistic odds ratios.",
  "This is a supplemental CCW/IPCW analysis within the eICU ICU-admission anchored arm2 analysis set.",
  "It is not a same-definition MIMIC external replication because eICU suspected-infection t0 cannot be recovered."
)
writeLines(report_lines, out_report)

message("Wrote coefficients: ", out_coef)
message("Wrote contrast: ", out_contrast)
message("Wrote risks: ", out_risk)
message("Wrote diagnostics: ", out_diag)
message("Wrote report: ", out_report)
print(contrast_pooled)
