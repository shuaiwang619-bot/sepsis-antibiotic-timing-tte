# eICU formal_v1 step 10: arm2 wide-table pooled logistic main model
# MIMIC counterpart: 10_formal_v1_ccw_pooled_logistic_main.R
#
# Scope:
#   eICU directional validation uses an ICU-admission anchored wide-table
#   arm2 comparison before any long-table CCW extension:
#     0-3h vs 3-12h antibiotic start, among post-t0 0-12h treated patients.
#
# Input:
#   outputs/mice/eicu_formal_v1_arm2_model_dataset_mice_m5_model_ready.csv
#
# Outputs:
#   outputs/models/eicu_formal_v1_arm2_model_pooled_terms.csv
#   outputs/models/eicu_formal_v1_arm2_model_exposure_summary.csv
#   outputs/models/eicu_formal_v1_arm2_model_qc_by_imp.csv
#   outputs/models/eicu_formal_v1_arm2_model_fit_diagnostics.csv
#   outputs/models/eicu_formal_v1_arm2_model_formula_plan.csv
#   outputs/models/eicu_formal_v1_arm2_model_report.txt

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
input_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "mice", "eicu_formal_v1_arm2_model_dataset_mice_m5_model_ready.csv")
}
run_glmm <- if (length(args) >= 3) {
  tolower(args[3]) %in% c("1", "true", "yes", "y", "glmm")
} else {
  FALSE
}
glmm_nagq <- if (length(args) >= 4) as.integer(args[4]) else 0L
glmm_maxfun <- if (length(args) >= 5) as.integer(args[5]) else 50000L

out_dir <- file.path(base_dir, "outputs", "models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_terms <- file.path(out_dir, "eicu_formal_v1_arm2_model_pooled_terms.csv")
out_exposure <- file.path(out_dir, "eicu_formal_v1_arm2_model_exposure_summary.csv")
out_qc <- file.path(out_dir, "eicu_formal_v1_arm2_model_qc_by_imp.csv")
out_fit_diag <- file.path(out_dir, "eicu_formal_v1_arm2_model_fit_diagnostics.csv")
out_formula_plan <- file.path(out_dir, "eicu_formal_v1_arm2_model_formula_plan.csv")
out_reference_levels <- file.path(out_dir, "eicu_formal_v1_arm2_model_reference_levels.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_arm2_model_report.txt")

if (run_glmm && !requireNamespace("lme4", quietly = TRUE)) {
  message("Package 'lme4' is not installed; hospital random-intercept GLMM will be skipped.")
}

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

as_int01 <- function(z) {
  znum <- suppressWarnings(as.integer(as.character(z)))
  znum[!(znum %in% c(0L, 1L))] <- NA_integer_
  znum
}

mode_ref <- function(z) {
  zz <- as.character(z)
  zz <- zz[!is.na(zz)]
  if (length(zz) == 0) return(NA_character_)
  names(sort(table(zz), decreasing = TRUE))[1]
}

set_factor_ref <- function(z, preferred_ref = NULL, missing_level = "Missing") {
  zz <- trimws(as.character(z))
  zz[is_miss(zz)] <- missing_level
  lv <- names(sort(table(zz), decreasing = TRUE))
  if (length(lv) == 0) lv <- missing_level
  ref <- if (!is.null(preferred_ref) && preferred_ref %in% lv) preferred_ref else lv[1]
  factor(zz, levels = c(ref, setdiff(sort(lv), ref)))
}

has_variation <- function(v) {
  if (is.factor(v)) {
    return(nlevels(droplevels(v)) > 1L)
  }
  vv <- v[!is.na(v)]
  length(unique(vv)) > 1L
}

make_formula <- function(outcome, terms, random_intercept = FALSE) {
  rhs <- paste(terms, collapse = " + ")
  if (random_intercept) rhs <- paste(rhs, "+ (1 | hospitalid)")
  stats::as.formula(paste(outcome, "~", rhs))
}

fit_capture <- function(expr) {
  warnings <- character()
  value <- NULL
  err <- NULL
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )
  list(fit = value, warnings = unique(warnings), error = err)
}

fixed_coef <- function(fit) {
  if (inherits(fit, "merMod")) return(lme4::fixef(fit))
  stats::coef(fit)
}

fixed_vcov <- function(fit) {
  as.matrix(stats::vcov(fit))
}

pool_fixed_effects <- function(fit_items, model_id) {
  ok <- vapply(fit_items, function(x) !is.null(x$fit), logical(1))
  fit_items <- fit_items[ok]
  if (length(fit_items) == 0) return(data.frame())

  terms <- Reduce(intersect, lapply(fit_items, function(x) names(fixed_coef(x$fit))))
  beta_mat <- do.call(cbind, lapply(fit_items, function(x) fixed_coef(x$fit)[terms]))
  vcov_list <- lapply(fit_items, function(x) fixed_vcov(x$fit)[terms, terms, drop = FALSE])
  u_mat <- do.call(cbind, lapply(vcov_list, diag))

  keep <- rowSums(!is.finite(beta_mat) | is.na(beta_mat)) == 0 &
    rowSums(!is.finite(u_mat) | is.na(u_mat)) == 0
  terms <- terms[keep]
  beta_mat <- beta_mat[keep, , drop = FALSE]
  u_mat <- u_mat[keep, , drop = FALSE]

  m <- ncol(beta_mat)
  qbar <- rowMeans(beta_mat)
  ubar <- rowMeans(u_mat)
  bvar <- if (m > 1L) apply(beta_mat, 1, stats::var) else rep(0, length(qbar))
  total_var <- ubar + (1 + 1 / m) * bvar
  se <- sqrt(total_var)
  df <- rep(Inf, length(qbar))
  has_between <- bvar > 1e-12
  df[has_between] <- (m - 1) * (1 + ubar[has_between] / ((1 + 1 / m) * bvar[has_between]))^2
  crit <- ifelse(is.finite(df), stats::qt(0.975, df = df), stats::qnorm(0.975))
  z_or_t <- qbar / se
  p_value <- ifelse(
    is.finite(df),
    2 * stats::pt(abs(z_or_t), df = df, lower.tail = FALSE),
    2 * stats::pnorm(abs(z_or_t), lower.tail = FALSE)
  )

  data.frame(
    model_id = model_id,
    term = terms,
    estimate = as.numeric(qbar),
    std_error = as.numeric(se),
    df = as.numeric(df),
    statistic = as.numeric(z_or_t),
    p_value = as.numeric(p_value),
    conf_low = as.numeric(qbar - crit * se),
    conf_high = as.numeric(qbar + crit * se),
    or = as.numeric(exp(qbar)),
    or_low = as.numeric(exp(qbar - crit * se)),
    or_high = as.numeric(exp(qbar + crit * se)),
    m_used = m,
    stringsAsFactors = FALSE
  )
}

fit_model_by_imp <- function(dat, formula, model_id, model_type = c("glm", "glmer")) {
  model_type <- match.arg(model_type)
  imps <- sort(unique(dat$.imp))
  fits <- vector("list", length(imps))
  names(fits) <- paste0("imp", imps)
  diag_rows <- vector("list", length(imps))

  for (i in seq_along(imps)) {
    d <- droplevels(dat[dat$.imp == imps[i], , drop = FALSE])
    if (model_type == "glm") {
      res <- fit_capture(stats::glm(formula, data = d, family = stats::binomial()))
    } else {
      res <- fit_capture(lme4::glmer(
        formula,
        data = d,
        family = stats::binomial(),
        nAGQ = glmm_nagq,
        control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = glmm_maxfun))
      ))
    }
    fits[[i]] <- res

    singular <- NA
    hospital_var <- NA_real_
    if (!is.null(res$fit) && inherits(res$fit, "merMod")) {
      singular <- lme4::isSingular(res$fit, tol = 1e-4)
      vc <- as.data.frame(lme4::VarCorr(res$fit))
      hospital_var <- vc$vcov[vc$grp == "hospitalid"][1]
    }

    diag_rows[[i]] <- data.frame(
      model_id = model_id,
      model_type = model_type,
      imp = imps[i],
      fit_ok = !is.null(res$fit),
      nobs = if (!is.null(res$fit)) stats::nobs(res$fit) else NA_integer_,
      aic = if (!is.null(res$fit)) stats::AIC(res$fit) else NA_real_,
      logLik = if (!is.null(res$fit)) as.numeric(stats::logLik(res$fit)) else NA_real_,
      singular = singular,
      hospital_random_intercept_var = hospital_var,
      warnings = paste(res$warnings, collapse = " | "),
      error = ifelse(is.null(res$error), "", res$error),
      stringsAsFactors = FALSE
    )
  }

  list(fits = fits, diagnostics = do.call(rbind, diag_rows))
}

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
}

x <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c(
  ".imp",
  "patientunitstayid",
  "uniquepid",
  "hospitalid",
  "death_30d_inhosp",
  "arm2_abx_timing",
  "arm2_late_3_12h",
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "hosp_zero_abx_reporting_ind",
  "age_model",
  "apache_score_iva_model",
  "apache_score_iva_model_missing_ind",
  "vent_model",
  "vent_model_missing_ind",
  "intubated_model",
  "intubated_model_missing_ind",
  "dialysis_model",
  "dialysis_model_missing_ind",
  "diabetes_model",
  "diabetes_model_missing_ind",
  "cirrhosis_model",
  "cirrhosis_model_missing_ind",
  "immunosuppression_model",
  "immunosuppression_model_missing_ind",
  "hepaticfailure_model",
  "hepaticfailure_model_missing_ind",
  "metastaticcancer_model",
  "metastaticcancer_model_missing_ind"
)
missing_required <- setdiff(required_cols, names(x))
if (length(missing_required) > 0) {
  stop("Missing required column(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
}

d <- data.frame(
  .imp = as.integer(to_num(x$.imp)),
  patientunitstayid = x$patientunitstayid,
  uniquepid = x$uniquepid,
  hospitalid = x$hospitalid,
  death_30d_inhosp = as_int01(x$death_30d_inhosp),
  arm2_abx_timing = set_factor_ref(x$arm2_abx_timing, preferred_ref = "0-3h"),
  arm2_late_3_12h = as_int01(x$arm2_late_3_12h),
  gender_factor = set_factor_ref(x$gender_factor, preferred_ref = "Male"),
  ethnicity_group = set_factor_ref(x$ethnicity_group, preferred_ref = "Caucasian"),
  unit_admit_source_group = set_factor_ref(x$unit_admit_source_group),
  unit_type_group = set_factor_ref(x$unit_type_group),
  hosp_window_volume_bucket_factor = set_factor_ref(x$hosp_window_volume_bucket_factor, preferred_ref = ">30"),
  hosp_zero_abx_reporting_ind = as_int01(x$hosp_zero_abx_reporting_ind),
  age_model = to_num(x$age_model),
  apache_score_iva_model = to_num(x$apache_score_iva_model),
  apache_score_iva_model_missing_ind = as_int01(x$apache_score_iva_model_missing_ind),
  vent_model = factor(as_int01(x$vent_model), levels = c(0, 1)),
  vent_model_missing_ind = as_int01(x$vent_model_missing_ind),
  intubated_model = factor(as_int01(x$intubated_model), levels = c(0, 1)),
  intubated_model_missing_ind = as_int01(x$intubated_model_missing_ind),
  dialysis_model = factor(as_int01(x$dialysis_model), levels = c(0, 1)),
  dialysis_model_missing_ind = as_int01(x$dialysis_model_missing_ind),
  diabetes_model = factor(as_int01(x$diabetes_model), levels = c(0, 1)),
  diabetes_model_missing_ind = as_int01(x$diabetes_model_missing_ind),
  cirrhosis_model = factor(as_int01(x$cirrhosis_model), levels = c(0, 1)),
  cirrhosis_model_missing_ind = as_int01(x$cirrhosis_model_missing_ind),
  immunosuppression_model = factor(as_int01(x$immunosuppression_model), levels = c(0, 1)),
  immunosuppression_model_missing_ind = as_int01(x$immunosuppression_model_missing_ind),
  hepaticfailure_model = factor(as_int01(x$hepaticfailure_model), levels = c(0, 1)),
  hepaticfailure_model_missing_ind = as_int01(x$hepaticfailure_model_missing_ind),
  metastaticcancer_model = factor(as_int01(x$metastaticcancer_model), levels = c(0, 1)),
  metastaticcancer_model_missing_ind = as_int01(x$metastaticcancer_model_missing_ind),
  stringsAsFactors = FALSE
)

d <- d[!is.na(d$.imp) & d$.imp >= 1L, , drop = FALSE]
d <- d[!is.na(d$death_30d_inhosp) & !is.na(d$arm2_late_3_12h), , drop = FALSE]
d$hospitalid <- factor(d$hospitalid)

imps <- sort(unique(d$.imp))
if (length(imps) < 2L) {
  stop("Expected at least two completed imputations; found: ", paste(imps, collapse = ", "), call. = FALSE)
}

qc_by_imp <- do.call(rbind, lapply(imps, function(ii) {
  z <- d[d$.imp == ii, , drop = FALSE]
  data.frame(
    imp = ii,
    rows_n = nrow(z),
    distinct_stays = length(unique(z$patientunitstayid)),
    n_hosp = length(unique(z$hospitalid)),
    death_n = sum(z$death_30d_inhosp == 1, na.rm = TRUE),
    death_pct = round(100 * mean(z$death_30d_inhosp == 1, na.rm = TRUE), 2),
    early_0_3h_n = sum(z$arm2_late_3_12h == 0, na.rm = TRUE),
    late_3_12h_n = sum(z$arm2_late_3_12h == 1, na.rm = TRUE),
    apache_score_missing_n = sum(is.na(z$apache_score_iva_model)),
    vent_missing_n = sum(is.na(z$vent_model)),
    diabetes_missing_n = sum(is.na(z$diabetes_model)),
    stringsAsFactors = FALSE
  )
}))
write.csv(qc_by_imp, out_qc, row.names = FALSE, na = "")

reference_vars <- c(
  "arm2_abx_timing",
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "hospitalid"
)
reference_levels <- do.call(rbind, lapply(reference_vars, function(v) {
  data.frame(
    variable = v,
    reference_level = levels(d[[v]])[1],
    n_levels = nlevels(droplevels(d[[v]])),
    levels = paste(levels(droplevels(d[[v]])), collapse = " | "),
    stringsAsFactors = FALSE
  )
}))
write.csv(reference_levels, out_reference_levels, row.names = FALSE, na = "")

base_terms <- c(
  "arm2_late_3_12h",
  "age_model",
  "apache_score_iva_model",
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "hosp_zero_abx_reporting_ind",
  "diabetes_model",
  "apache_score_iva_model_missing_ind",
  "diabetes_model_missing_ind"
)

support_terms <- c(
  base_terms,
  "vent_model",
  "intubated_model",
  "dialysis_model"
)

rare_terms <- c(
  base_terms,
  "cirrhosis_model",
  "immunosuppression_model",
  "hepaticfailure_model",
  "metastaticcancer_model",
  "cirrhosis_model_missing_ind"
)

crude_terms <- c("arm2_late_3_12h")

term_sets <- list(
  crude_glm = crude_terms,
  main_glm = base_terms,
  support_expanded_glm = support_terms,
  rare_comorbidity_glm = rare_terms
)

if (run_glmm && requireNamespace("lme4", quietly = TRUE)) {
  term_sets$main_hospital_random_intercept_glmm <- base_terms
}

formula_plan <- do.call(rbind, lapply(names(term_sets), function(model_id) {
  requested <- term_sets[[model_id]]
  dropped <- requested[!vapply(requested, function(v) has_variation(d[[v]]), logical(1))]
  kept <- setdiff(requested, dropped)
  formula_txt <- paste(deparse(make_formula(
    "death_30d_inhosp",
    kept,
    random_intercept = identical(model_id, "main_hospital_random_intercept_glmm")
  )), collapse = " ")
  data.frame(
    model_id = model_id,
    requested_terms = paste(requested, collapse = " + "),
    kept_terms = paste(kept, collapse = " + "),
    dropped_nonvarying_terms = paste(dropped, collapse = " + "),
    formula = formula_txt,
    stringsAsFactors = FALSE
  )
}))
write.csv(formula_plan, out_formula_plan, row.names = FALSE, na = "")

all_term_results <- list()
all_diag <- list()

for (model_id in names(term_sets)) {
  requested <- term_sets[[model_id]]
  kept <- requested[vapply(requested, function(v) has_variation(d[[v]]), logical(1))]
  if (length(kept) == 0) next
  is_glmm <- identical(model_id, "main_hospital_random_intercept_glmm")
  f <- make_formula("death_30d_inhosp", kept, random_intercept = is_glmm)
  model_type <- if (is_glmm) "glmer" else "glm"
  fit_obj <- fit_model_by_imp(d, f, model_id = model_id, model_type = model_type)
  all_diag[[model_id]] <- fit_obj$diagnostics
  all_term_results[[model_id]] <- pool_fixed_effects(fit_obj$fits, model_id)
}

pooled_terms <- do.call(rbind, all_term_results)
fit_diagnostics <- do.call(rbind, all_diag)

if (is.null(pooled_terms) || nrow(pooled_terms) == 0) {
  stop("No model terms were pooled successfully.", call. = FALSE)
}

pooled_terms$is_exposure_contrast <- pooled_terms$term == "arm2_late_3_12h"
write.csv(pooled_terms, out_terms, row.names = FALSE, na = "")
write.csv(fit_diagnostics, out_fit_diag, row.names = FALSE, na = "")

exposure_summary <- pooled_terms[pooled_terms$is_exposure_contrast, , drop = FALSE]
exposure_summary$contrast <- "3-12h vs 0-3h"
exposure_summary$interpretation <- ifelse(
  exposure_summary$or > 1,
  "OR above 1 favors higher odds of 30-day in-hospital death in 3-12h",
  "OR below 1 favors lower odds of 30-day in-hospital death in 3-12h"
)
write.csv(exposure_summary, out_exposure, row.names = FALSE, na = "")

report_lines <- c(
  "eICU formal_v1 arm2 wide-table pooled logistic model report",
  paste("input:", input_file),
  "",
  "analysis set by completed imputation:",
  capture.output(print(qc_by_imp, row.names = FALSE)),
  "",
  "primary contrast:",
  "3-12h antibiotic start vs 0-3h antibiotic start; outcome = 30-day in-hospital death.",
  "",
  "model hierarchy:",
  "crude_glm: exposure only.",
  "main_glm: age, APACHE IVa score, demographics, unit factors, hospital reporting-volume factor, diabetes, and missing indicators.",
  if (run_glmm) {
    paste0("main_hospital_random_intercept_glmm: same fixed effects as main_glm plus (1 | hospitalid); nAGQ=", glmm_nagq, ", maxfun=", glmm_maxfun, ".")
  } else {
    "main_hospital_random_intercept_glmm: skipped in this fast run; pass TRUE as the third argument to run it."
  },
  "support_expanded_glm: main_glm plus vent/intubated/dialysis.",
  "rare_comorbidity_glm: main_glm plus rare comorbidity flags.",
  "",
  "formula plan:",
  capture.output(print(formula_plan[, c("model_id", "dropped_nonvarying_terms", "formula")], row.names = FALSE)),
  "",
  "reference levels:",
  capture.output(print(reference_levels, row.names = FALSE)),
  "",
  "exposure summary:",
  capture.output(print(exposure_summary[, c("model_id", "contrast", "or", "or_low", "or_high", "p_value", "m_used")], row.names = FALSE)),
  "",
  "fit diagnostics:",
  capture.output(print(fit_diagnostics, row.names = FALSE)),
  "",
  "outputs:",
  out_terms,
  out_exposure,
  out_qc,
  out_fit_diag,
  out_formula_plan,
  out_reference_levels
)
writeLines(report_lines, out_report)

message("Done. Exposure summary:")
print(exposure_summary[, c("model_id", "contrast", "or", "or_low", "or_high", "p_value", "m_used")], row.names = FALSE)
message("Report: ", out_report)
