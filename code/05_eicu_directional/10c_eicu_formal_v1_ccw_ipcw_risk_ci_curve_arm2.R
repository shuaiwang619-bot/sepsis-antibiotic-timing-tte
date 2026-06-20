# eICU formal_v1 step 10c: arm2 CCW/IPCW risk CI and cumulative risk curve
# MIMIC counterpart: supplemental diagnostics after 10_formal_v1_ccw_pooled_logistic_main.R
#
# Scope:
#   Add the final reviewer-proof diagnostics requested for the eICU arm2
#   exploratory CCW/IPCW supplement:
#     1) zero-period clone audit;
#     2) model-standardized risk, RD, and RR with MI-pooled confidence intervals;
#     3) IPCW-weighted cumulative risk curve.

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
weights_file <- if (length(args) >= 3) {
  args[3]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_ipcw_final_weights_m5.csv")
}
time_weights_file <- if (length(args) >= 4) {
  args[4]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_ipcw_time_weights_m5.csv")
}
weight_col <- if (length(args) >= 5) args[5] else "ipcw_stabilized"

out_dir <- file.path(base_dir, "outputs", "ccw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_zero <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_zero_period_clone_audit.csv")
out_risk <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_30d_risk_rd_rr_ci.csv")
out_curve <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_weighted_cumulative_risk_curve.csv")
out_curve_png <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_weighted_cumulative_risk_curve.png")
out_curve_pdf <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_weighted_cumulative_risk_curve.pdf")
out_report <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_risk_ci_curve_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

logit_inv <- function(eta) {
  stats::plogis(pmin(pmax(eta, -35), 35))
}

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

pool_scalar <- function(q, u, term, transform = c("none", "exp")) {
  transform <- match.arg(transform)
  q <- as.numeric(q)
  u <- as.numeric(u)
  ok <- is.finite(q) & is.finite(u) & !is.na(q) & !is.na(u) & u >= 0
  q <- q[ok]
  u <- u[ok]
  m <- length(q)
  if (m == 0L) {
    return(data.table(
      estimand = term, m = 0L, estimate = NA_real_, std_error = NA_real_,
      df = NA_real_, p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_
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
  p <- 2 * stats::pnorm(abs(qbar / se), lower.tail = FALSE)
  if (is.finite(df)) p <- 2 * stats::pt(abs(qbar / se), df = df, lower.tail = FALSE)
  out <- data.table(
    estimand = term,
    m = m,
    estimate = qbar,
    std_error = se,
    df = df,
    p_value = p,
    conf_low = qbar - crit * se,
    conf_high = qbar + crit * se
  )
  if (transform == "exp") {
    out[, `:=`(
      estimate = exp(estimate),
      conf_low = exp(conf_low),
      conf_high = exp(conf_high),
      scale = "ratio"
    )]
  } else {
    out[, scale := "risk"]
  }
  out
}

align_matrix <- function(mm, beta_names) {
  missing_cols <- setdiff(beta_names, colnames(mm))
  if (length(missing_cols) > 0) {
    add <- matrix(0, nrow = nrow(mm), ncol = length(missing_cols))
    colnames(add) <- missing_cols
    mm <- cbind(mm, add)
  }
  mm[, beta_names, drop = FALSE]
}

standardized_risk_gradient <- function(fit, vcov_mat, base_stay, arm_value, time_levels) {
  beta <- stats::coef(fit)
  beta_names <- names(beta)
  terms_no_resp <- stats::delete.response(stats::terms(fit))

  risk_sum <- 0
  grad_sum <- rep(0, length(beta))
  names(grad_sum) <- beta_names

  for (ii in seq_len(nrow(base_stay))) {
    nd <- base_stay[rep(ii, length(time_levels))]
    nd[, arm := factor(arm_value, levels = c("0-3h", "3-12h"))]
    nd[, time_axis := factor(time_levels, levels = time_levels)]
    mm <- stats::model.matrix(terms_no_resp, data = nd)
    mm <- align_matrix(mm, beta_names)
    p <- logit_inv(as.numeric(mm %*% beta))
    surv <- prod(1 - p)
    risk_i <- 1 - surv
    grad_i <- as.numeric(surv * colSums(mm * p))

    risk_sum <- risk_sum + risk_i
    grad_sum <- grad_sum + grad_i
  }

  n <- nrow(base_stay)
  risk <- risk_sum / n
  grad <- grad_sum / n
  var <- as.numeric(t(grad) %*% vcov_mat[beta_names, beta_names, drop = FALSE] %*% grad)
  list(risk = risk, grad = grad, variance = var)
}

stop_if_missing(pp_file, "Step 09 person-period table")
stop_if_missing(weights_file, "Step 08 final weights")
stop_if_missing(time_weights_file, "Step 08 time weights")

pp <- fread(pp_file, showProgress = FALSE)
weights <- fread(weights_file, showProgress = FALSE)
time_weights <- fread(time_weights_file, showProgress = FALSE)

need_pp <- c(
  ".imp", "clone_id", "patientunitstayid", "hospitalid",
  "arm", "time_axis", "time_start_h", "time_end_h", "event_30d_period", weight_col,
  "age_model", "apache_score_iva_model", "apache_score_iva_model_missing_ind",
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "diabetes_model_missing_ind"
)
need_weights <- c(
  ".imp", "clone_id", "patientunitstayid", "arm", "arm_id",
  "artificial_censor_h", "artificial_censor_reason", "strategy_adherent_by_delay"
)
need_time <- c(
  ".imp", "clone_id", "patientunitstayid", "arm", "arm_id", "t_hour",
  "artificial_censor_event", "ipcw_t"
)
missing_pp <- setdiff(need_pp, names(pp))
missing_weights <- setdiff(need_weights, names(weights))
missing_time <- setdiff(need_time, names(time_weights))
if (length(missing_pp) > 0) stop("Person-period file missing columns: ", paste(missing_pp, collapse = ", "), call. = FALSE)
if (length(missing_weights) > 0) stop("Weights file missing columns: ", paste(missing_weights, collapse = ", "), call. = FALSE)
if (length(missing_time) > 0) stop("Time-weights file missing columns: ", paste(missing_time, collapse = ", "), call. = FALSE)

pp[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  hospitalid = as.integer(to_num(hospitalid)),
  event_30d_period = as.integer(to_num(event_30d_period)),
  model_weight = to_num(get(weight_col)),
  time_start_h = to_num(time_start_h),
  time_end_h = to_num(time_end_h),
  age_model = to_num(age_model),
  apache_score_iva_model = to_num(apache_score_iva_model),
  apache_score_iva_model_missing_ind = as.integer(to_num(apache_score_iva_model_missing_ind)),
  hosp_zero_abx_reporting_ind = as.integer(to_num(hosp_zero_abx_reporting_ind)),
  diabetes_model = as.integer(to_num(diabetes_model)),
  diabetes_model_missing_ind = as.integer(to_num(diabetes_model_missing_ind))
)]
weights[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  artificial_censor_h = to_num(artificial_censor_h),
  strategy_adherent_by_delay = as.logical(strategy_adherent_by_delay)
)]
time_weights[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  t_hour = as.integer(to_num(t_hour)),
  artificial_censor_event = as.integer(to_num(artificial_censor_event)),
  ipcw_t = to_num(ipcw_t)
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

if (pp[, any(!is.finite(model_weight) | is.na(model_weight) | model_weight <= 0)]) {
  stop("Non-positive, missing, or non-finite person-period model weights detected.", call. = FALSE)
}

# 1) Zero-period clone audit.
pp_clone_ids <- unique(pp[, .(.imp, clone_id)])
zero_candidates <- weights[!pp_clone_ids, on = c(".imp", "clone_id")]
zero_candidates <- zero_candidates[
  !strategy_adherent_by_delay &
    arm == "3-12h" &
    !is.na(artificial_censor_h) &
    artificial_censor_h == 0
]

zero_audit <- zero_candidates[, .(
  zero_period_clones = .N,
  distinct_stays = uniqueN(patientunitstayid)
), by = .imp][order(.imp)]

zero_time <- time_weights[
  zero_candidates[, .(.imp, clone_id)],
  on = c(".imp", "clone_id"),
  nomatch = 0
]
zero_time_audit <- zero_time[, .(
  rows_in_time_weight_model = .N,
  rows_at_hour0 = sum(t_hour == 0),
  censor_events_at_hour0 = sum(t_hour == 0 & artificial_censor_event == 1L),
  any_nonzero_hour_rows = sum(t_hour != 0)
), by = .imp][order(.imp)]

zero_pp_audit <- pp[
  zero_candidates[, .(.imp, clone_id)],
  on = c(".imp", "clone_id"),
  nomatch = 0,
  .(person_period_rows = .N),
  by = .EACHI
][, .(
  clones_with_any_person_period = sum(person_period_rows > 0),
  person_period_rows = sum(person_period_rows)
), by = .imp][order(.imp)]

zero_audit_out <- merge(zero_audit, zero_time_audit, by = ".imp", all = TRUE)
zero_audit_out <- merge(zero_audit_out, zero_pp_audit, by = ".imp", all = TRUE)
zero_audit_out[is.na(clones_with_any_person_period), clones_with_any_person_period := 0L]
zero_audit_out[is.na(person_period_rows), person_period_rows := 0L]
zero_audit_out[, zero_period_clones_entered_censor_model := (
  zero_period_clones == rows_at_hour0 &
    zero_period_clones == censor_events_at_hour0 &
    clones_with_any_person_period == 0L
)]
fwrite(zero_audit_out, out_zero)

if (zero_audit_out[, any(!zero_period_clones_entered_censor_model)]) {
  print(zero_audit_out)
  stop("Zero-period clone audit failed.", call. = FALSE)
}

# 2) Model-standardized risk, RD, and RR with robust delta-method variance.
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
  stop("Model columns contain NA.", call. = FALSE)
}

risk_imp <- list()
fit_diag <- list()

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
  beta_names <- names(stats::coef(fit))

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

  r_early <- standardized_risk_gradient(fit, vc, base_stay, "0-3h", time_levels)
  r_late <- standardized_risk_gradient(fit, vc, base_stay, "3-12h", time_levels)

  grad_rd <- r_late$grad - r_early$grad
  var_rd <- as.numeric(t(grad_rd) %*% vc[beta_names, beta_names, drop = FALSE] %*% grad_rd)

  grad_log_rr <- r_late$grad / r_late$risk - r_early$grad / r_early$risk
  var_log_rr <- as.numeric(t(grad_log_rr) %*% vc[beta_names, beta_names, drop = FALSE] %*% grad_log_rr)

  risk_imp[[as.character(ii)]] <- rbindlist(list(
    data.table(.imp = ii, estimand = "risk_0_3h", q = r_early$risk, u = r_early$variance, scale = "risk"),
    data.table(.imp = ii, estimand = "risk_3_12h", q = r_late$risk, u = r_late$variance, scale = "risk"),
    data.table(.imp = ii, estimand = "risk_difference_3_12h_minus_0_3h", q = r_late$risk - r_early$risk, u = var_rd, scale = "risk_difference"),
    data.table(.imp = ii, estimand = "log_risk_ratio_3_12h_vs_0_3h", q = log(r_late$risk / r_early$risk), u = var_log_rr, scale = "log_ratio")
  ))

  fit_diag[[as.character(ii)]] <- data.table(
    .imp = ii,
    rows = nrow(fit_dat),
    clusters = uniqueN(fit_dat$patientunitstayid),
    coefficients = length(stats::coef(fit)),
    converged = isTRUE(fit$converged),
    formula = paste(deparse(main_formula, width.cutoff = 500L), collapse = " ")
  )
}

risk_imp <- rbindlist(risk_imp)
fit_diag <- rbindlist(fit_diag)

risk_pooled <- rbindlist(list(
  pool_scalar(
    risk_imp[estimand == "risk_0_3h", q],
    risk_imp[estimand == "risk_0_3h", u],
    "risk_0_3h",
    transform = "none"
  ),
  pool_scalar(
    risk_imp[estimand == "risk_3_12h", q],
    risk_imp[estimand == "risk_3_12h", u],
    "risk_3_12h",
    transform = "none"
  ),
  pool_scalar(
    risk_imp[estimand == "risk_difference_3_12h_minus_0_3h", q],
    risk_imp[estimand == "risk_difference_3_12h_minus_0_3h", u],
    "risk_difference_3_12h_minus_0_3h",
    transform = "none"
  ),
  pool_scalar(
    risk_imp[estimand == "log_risk_ratio_3_12h_vs_0_3h", q],
    risk_imp[estimand == "log_risk_ratio_3_12h_vs_0_3h", u],
    "risk_ratio_3_12h_vs_0_3h",
    transform = "exp"
  )
), fill = TRUE)

risk_out <- rbindlist(list(
  cbind(section = "pooled", risk_pooled),
  cbind(section = "per_imp", risk_imp)
), fill = TRUE)
fwrite(risk_out, out_risk)

# 3) IPCW-weighted cumulative risk curve.
curve_imp <- pp[
  ,
  .(
    weighted_risk_set = sum(model_weight, na.rm = TRUE),
    weighted_events = sum(model_weight * event_30d_period, na.rm = TRUE),
    unweighted_rows = .N,
    unweighted_events = sum(event_30d_period, na.rm = TRUE)
  ),
  by = .(.imp, arm, time_start_h, time_end_h, time_axis)
][order(.imp, arm, time_start_h)]

curve_imp[, hazard := fifelse(weighted_risk_set > 0, weighted_events / weighted_risk_set, 0)]
curve_imp[, hazard := pmin(pmax(hazard, 0), 1)]
curve_imp[, survival := cumprod(1 - hazard), by = .(.imp, arm)]
curve_imp[, cumulative_risk := 1 - survival]

curve_summary <- curve_imp[, .(
  weighted_risk_set_mean = mean(weighted_risk_set),
  weighted_events_mean = mean(weighted_events),
  hazard_mean = mean(hazard),
  cumulative_risk_mean = mean(cumulative_risk),
  cumulative_risk_min = min(cumulative_risk),
  cumulative_risk_max = max(cumulative_risk)
), by = .(arm, time_start_h, time_end_h, time_axis)][order(arm, time_start_h)]

curve_out <- rbindlist(list(
  cbind(section = "summary", curve_summary),
  cbind(section = "per_imp", curve_imp)
), fill = TRUE)
fwrite(curve_out, out_curve)

plot_curve <- function(curve_summary, curve_imp, out_png, out_pdf) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = curve_imp,
        ggplot2::aes(x = time_end_h / 24, y = cumulative_risk, group = interaction(.imp, arm), color = arm),
        alpha = 0.25,
        linewidth = 0.35
      ) +
      ggplot2::geom_line(
        data = curve_summary,
        ggplot2::aes(x = time_end_h / 24, y = cumulative_risk_mean, color = arm),
        linewidth = 1.1
      ) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::scale_x_continuous(breaks = c(0, 1, 7, 14, 21, 30), limits = c(0, 30)) +
      ggplot2::labs(
        x = "Days from ICU admission",
        y = "IPCW-weighted cumulative 30-day in-hospital mortality",
        color = "Strategy"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "top",
        panel.grid.minor = ggplot2::element_blank()
      )
    ggplot2::ggsave(out_png, p, width = 7, height = 4.5, dpi = 300)
    ggplot2::ggsave(out_pdf, p, width = 7, height = 4.5)
  } else {
    grDevices::png(out_png, width = 2100, height = 1350, res = 300)
    plot(
      NA,
      xlim = c(0, 30),
      ylim = c(0, max(curve_summary$cumulative_risk_mean, na.rm = TRUE) * 1.1),
      xlab = "Days from ICU admission",
      ylab = "IPCW-weighted cumulative 30-day in-hospital mortality"
    )
    cols <- c("0-3h" = "#2B6CB0", "3-12h" = "#C2410C")
    for (aa in names(cols)) {
      zz <- curve_summary[as.character(arm) == aa]
      lines(zz$time_end_h / 24, zz$cumulative_risk_mean, col = cols[[aa]], lwd = 2)
    }
    legend("topleft", legend = names(cols), col = cols, lwd = 2, bty = "n")
    grDevices::dev.off()

    grDevices::pdf(out_pdf, width = 7, height = 4.5)
    plot(
      NA,
      xlim = c(0, 30),
      ylim = c(0, max(curve_summary$cumulative_risk_mean, na.rm = TRUE) * 1.1),
      xlab = "Days from ICU admission",
      ylab = "IPCW-weighted cumulative 30-day in-hospital mortality"
    )
    cols <- c("0-3h" = "#2B6CB0", "3-12h" = "#C2410C")
    for (aa in names(cols)) {
      zz <- curve_summary[as.character(arm) == aa]
      lines(zz$time_end_h / 24, zz$cumulative_risk_mean, col = cols[[aa]], lwd = 2)
    }
    legend("topleft", legend = names(cols), col = cols, lwd = 2, bty = "n")
    grDevices::dev.off()
  }
}

plot_curve(curve_summary, curve_imp, out_curve_png, out_curve_pdf)

report_lines <- c(
  "eICU formal_v1 step 10c arm2 CCW/IPCW risk CI and cumulative risk curve",
  paste("person_period:", pp_file),
  paste("weights:", weights_file),
  paste("time_weights:", time_weights_file),
  paste("weight_col:", weight_col),
  "",
  "zero-period clone audit:",
  capture.output(print(zero_audit_out)),
  "",
  "model fit diagnostics:",
  capture.output(print(fit_diag)),
  "",
  "risk/RD/RR pooled estimates:",
  capture.output(print(risk_pooled)),
  "",
  "weighted cumulative risk at 30 days:",
  capture.output(print(curve_summary[time_end_h == max(time_end_h), .(
    arm, cumulative_risk_mean, cumulative_risk_min, cumulative_risk_max
  )])),
  "",
  "notes:",
  "RD and RR confidence intervals use delta-method robust variance within each imputation and Rubin pooling across imputations.",
  "The cumulative risk curve is nonparametric over the CCW/IPCW person-period table: weighted events divided by weighted risk set in each interval.",
  "Zero-period 3-12h clones are retained in the censoring-weight model at t_hour=0 and contribute no outcome person-time."
)
writeLines(report_lines, out_report)

message("Wrote zero-period audit: ", out_zero)
message("Wrote risk/RD/RR CI: ", out_risk)
message("Wrote cumulative risk curve data: ", out_curve)
message("Wrote cumulative risk curve PNG: ", out_curve_png)
message("Wrote cumulative risk curve PDF: ", out_curve_pdf)
message("Wrote report: ", out_report)
print(risk_pooled)
