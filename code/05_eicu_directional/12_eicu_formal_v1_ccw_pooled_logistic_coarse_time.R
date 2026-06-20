# eICU formal_v1 step 12: >12-72h coarse-time sensitivity model
# MIMIC counterpart: 12_formal_v1_ccw_pooled_logistic_coarse_time.R
#
# Scope:
#   Directional validation sensitivity:
#     >12-72h antibiotic start vs 0-3h antibiotic start.
#
# Important:
#   This script needs the full 21,326-row model-ready wide table, not the
#   3,225-row arm2 file. If the default CSV has only arm2 rows, re-export
#   public.eicu_formal_v1_analysis_main_model_ready_wide from Navicat first.
#
# Input:
#   outputs/wide_tables/eicu_formal_v1_analysis_main_model_ready_wide.csv
#
# Outputs:
#   outputs/sensitivity/eicu_formal_v1_12_72_model_dataset_mice_m5_model_ready.csv
#   outputs/sensitivity/eicu_formal_v1_12_72_exposure_summary.csv
#   outputs/sensitivity/eicu_formal_v1_12_72_fit_diagnostics.csv
#   outputs/sensitivity/eicu_formal_v1_12_72_report.txt

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
  file.path(base_dir, "outputs", "wide_tables", "eicu_formal_v1_analysis_main_model_ready_wide.csv")
}
m_imp <- if (length(args) >= 3) as.integer(args[3]) else 5L
maxit <- if (length(args) >= 4) as.integer(args[4]) else 20L
seed <- if (length(args) >= 5) as.integer(args[5]) else 2025L
run_glmm <- if (length(args) >= 6) {
  tolower(args[6]) %in% c("1", "true", "yes", "y", "glmm")
} else {
  TRUE
}

out_dir <- file.path(base_dir, "outputs", "sensitivity")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_input <- file.path(out_dir, "eicu_formal_v1_12_72_mice_input.csv")
out_model_ready <- file.path(out_dir, "eicu_formal_v1_12_72_model_dataset_mice_m5_model_ready.csv")
out_missingness <- file.path(out_dir, "eicu_formal_v1_12_72_mice_missingness_report.csv")
out_method <- file.path(out_dir, "eicu_formal_v1_12_72_mice_method_matrix.csv")
out_terms <- file.path(out_dir, "eicu_formal_v1_12_72_pooled_terms.csv")
out_exposure <- file.path(out_dir, "eicu_formal_v1_12_72_exposure_summary.csv")
out_fit_diag <- file.path(out_dir, "eicu_formal_v1_12_72_fit_diagnostics.csv")
out_formula_plan <- file.path(out_dir, "eicu_formal_v1_12_72_formula_plan.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_12_72_report.txt")

if (!requireNamespace("mice", quietly = TRUE)) {
  stop("Package 'mice' is required.", call. = FALSE)
}
if (run_glmm && !requireNamespace("lme4", quietly = TRUE)) {
  message("Package 'lme4' is not installed; GLMM will be skipped.")
  run_glmm <- FALSE
}

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

clean_text <- function(z) {
  out <- trimws(as.character(z))
  out[is_miss(z)] <- NA_character_
  out
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

as_int01 <- function(z) {
  znum <- suppressWarnings(as.integer(as.character(z)))
  znum[!(znum %in% c(0L, 1L))] <- NA_integer_
  znum
}

gt0_or0 <- function(z) {
  znum <- to_num(z)
  as.integer(!is.na(znum) & znum > 0)
}

factor_with_missing <- function(z, missing_level = "Missing") {
  out <- clean_text(z)
  out[is.na(out)] <- missing_level
  droplevels(factor(out))
}

lump_small_levels <- function(z, min_n = 30L, other_level = "Other/rare", missing_level = "Missing") {
  f <- factor_with_missing(z, missing_level = missing_level)
  tab <- table(f, useNA = "no")
  rare <- names(tab)[tab < min_n & names(tab) != missing_level]
  out <- as.character(f)
  out[out %in% rare] <- other_level
  droplevels(factor(out))
}

unit_source_group <- function(z) {
  txt <- clean_text(z)
  safe <- txt
  safe[is.na(safe)] <- ""
  out <- ifelse(grepl("Emergency", safe, ignore.case = TRUE), "Emergency Department",
         ifelse(grepl("Direct", safe, ignore.case = TRUE), "Direct Admit",
         ifelse(grepl("Floor|Acute", safe, ignore.case = TRUE), "Floor/Acute Care",
         ifelse(grepl("Other Hospital|Other ICU", safe, ignore.case = TRUE), "Transfer",
         ifelse(grepl("Operating|PACU|Recovery", safe, ignore.case = TRUE), "Perioperative",
         ifelse(is.na(txt), "Missing", "Other"))))))
  factor(out, levels = c(
    "Emergency Department", "Direct Admit", "Floor/Acute Care",
    "Transfer", "Perioperative", "Other", "Missing"
  ))
}

unit_type_group <- function(z) {
  txt <- clean_text(z)
  safe <- txt
  safe[is.na(safe)] <- ""
  out <- ifelse(safe == "Med-Surg ICU", "Med-Surg ICU",
         ifelse(safe == "MICU", "MICU",
         ifelse(safe == "SICU", "SICU",
         ifelse(grepl("Cardiac|CCU|CTICU|CSICU", safe, ignore.case = TRUE),
                "Cardiac/Surgical Cardiac ICU",
                ifelse(is.na(txt), "Missing", "Other ICU")))))
  factor(out, levels = c(
    "Med-Surg ICU", "MICU", "SICU",
    "Cardiac/Surgical Cardiac ICU", "Other ICU", "Missing"
  ))
}

as_binary_factor <- function(z) {
  factor(as_int01(z), levels = c(0L, 1L))
}

completed_vector <- function(v) {
  if (is.factor(v)) {
    vv <- as.character(v)
    if (all(is.na(vv) | vv %in% c("0", "1"))) return(as.integer(vv))
    return(vv)
  }
  v
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
  if (is.factor(v)) return(nlevels(droplevels(v)) > 1L)
  vv <- v[!is.na(v)]
  length(unique(vv)) > 1L
}

fit_capture <- function(expr) {
  warnings <- character()
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

fixed_vcov <- function(fit) as.matrix(stats::vcov(fit))

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
  stat <- qbar / se
  p_value <- ifelse(
    is.finite(df),
    2 * stats::pt(abs(stat), df = df, lower.tail = FALSE),
    2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  )
  data.frame(
    model_id = model_id,
    term = terms,
    estimate = as.numeric(qbar),
    std_error = as.numeric(se),
    df = as.numeric(df),
    statistic = as.numeric(stat),
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

make_formula <- function(outcome, terms, random_intercept = FALSE) {
  rhs <- paste(terms, collapse = " + ")
  if (random_intercept) rhs <- paste(rhs, "+ (1 | hospitalid)")
  stats::as.formula(paste(outcome, "~", rhs))
}

fit_model_by_imp <- function(dat, formula, model_id, model_type = c("glm", "glmer")) {
  model_type <- match.arg(model_type)
  imps <- sort(unique(dat$.imp))
  fits <- vector("list", length(imps))
  diag_rows <- vector("list", length(imps))
  for (i in seq_along(imps)) {
    z <- droplevels(dat[dat$.imp == imps[i], , drop = FALSE])
    if (model_type == "glm") {
      res <- fit_capture(stats::glm(formula, data = z, family = stats::binomial()))
    } else {
      res <- fit_capture(lme4::glmer(
        formula,
        data = z,
        family = stats::binomial(),
        nAGQ = 0,
        control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
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
      singular = singular,
      hospital_random_intercept_var = hospital_var,
      warnings = paste(res$warnings, collapse = " | "),
      error = ifelse(is.null(res$error), "", res$error),
      stringsAsFactors = FALSE
    )
  }
  list(fits = fits, diagnostics = do.call(rbind, diag_rows))
}

first_existing <- function(dat, candidates, required = TRUE) {
  found <- candidates[candidates %in% names(dat)]
  if (length(found) > 0) return(found[1])
  if (required) stop("Missing required source column; tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  NA_character_
}

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
}

x <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
if (nrow(x) < 10000L || !"abx_window_unit_t0" %in% names(x)) {
  stop(
    "This input is not the full model-ready wide table. ",
    "Expected about 21,326 rows and column abx_window_unit_t0. ",
    "Re-export public.eicu_formal_v1_analysis_main_model_ready_wide from Navicat, ",
    "then save it as outputs/wide_tables/eicu_formal_v1_analysis_main_model_ready_wide.csv.",
    call. = FALSE
  )
}

required_cols <- c(
  "patientunitstayid", "hospitalid", "death_30d_inhosp", "abx_window_unit_t0",
  "apache_score_iva_clean", "apache_score_iva_missing_ind",
  "vent_apache_clean", "vent_apache_missing_ind",
  "intubated_apache_clean", "intubated_apache_missing_ind",
  "dialysis_apache_clean", "dialysis_apache_missing_ind",
  "diabetes_pred_clean", "diabetes_pred_missing_ind",
  "cirrhosis_pred_clean", "cirrhosis_pred_missing_ind",
  "immunosuppression_pred_clean", "immunosuppression_pred_missing_ind",
  "hepaticfailure_pred_clean", "hepaticfailure_pred_missing_ind",
  "metastaticcancer_pred_clean", "metastaticcancer_pred_missing_ind",
  "hosp_main_three_window_bucket", "hosp_zero_abx_reporting_ind"
)
missing_required <- setdiff(required_cols, names(x))
if (length(missing_required) > 0) {
  stop("Missing required column(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
}

age_col <- first_existing(x, c("age_num", "age", "age_model"))
gender_col <- first_existing(x, c("gender", "gender_model"))
ethnicity_col <- first_existing(x, c("ethnicity", "ethnicity_model"))
unit_admit_col <- first_existing(x, c("unitadmitsource", "unit_admit_source_model"))
unit_type_col <- first_existing(x, c("unittype", "unit_type_model"))
uniquepid_col <- first_existing(x, c("uniquepid"), required = FALSE)

d0 <- x[x$abx_window_unit_t0 %in% c("0-3h", ">12-72h") & !is.na(as_int01(x$death_30d_inhosp)), , drop = FALSE]
d0$sens_12_72_abx_timing <- ifelse(d0$abx_window_unit_t0 == "0-3h", "0-3h", ">12-72h")
d0$sens_late_12_72h <- ifelse(d0$abx_window_unit_t0 == ">12-72h", 1L, 0L)

if (nrow(d0) < 1000L) {
  stop("Sensitivity analysis set is unexpectedly small: ", nrow(d0), call. = FALSE)
}

age_raw <- to_num(d0[[age_col]])
age_model <- ifelse(!is.na(age_raw) & age_raw >= 18 & age_raw <= 120, age_raw, NA_real_)

y <- data.frame(
  patientunitstayid = d0$patientunitstayid,
  uniquepid = if (!is.na(uniquepid_col)) d0[[uniquepid_col]] else NA,
  hospitalid = d0$hospitalid,
  death_30d_inhosp = as_int01(d0$death_30d_inhosp),
  sens_12_72_abx_timing = factor(d0$sens_12_72_abx_timing, levels = c("0-3h", ">12-72h")),
  sens_late_12_72h = as.integer(d0$sens_late_12_72h),
  age_model = age_model,
  age_model_missing_ind = as.integer(is.na(age_model)),
  apache_score_iva_model = to_num(d0$apache_score_iva_clean),
  apache_score_iva_model_missing_ind = gt0_or0(d0$apache_score_iva_missing_ind),
  vent_model = as_binary_factor(d0$vent_apache_clean),
  vent_model_missing_ind = gt0_or0(d0$vent_apache_missing_ind),
  intubated_model = as_binary_factor(d0$intubated_apache_clean),
  intubated_model_missing_ind = gt0_or0(d0$intubated_apache_missing_ind),
  dialysis_model = as_binary_factor(d0$dialysis_apache_clean),
  dialysis_model_missing_ind = gt0_or0(d0$dialysis_apache_missing_ind),
  diabetes_model = as_binary_factor(d0$diabetes_pred_clean),
  diabetes_model_missing_ind = gt0_or0(d0$diabetes_pred_missing_ind),
  cirrhosis_model = as_binary_factor(d0$cirrhosis_pred_clean),
  cirrhosis_model_missing_ind = gt0_or0(d0$cirrhosis_pred_missing_ind),
  immunosuppression_model = as_binary_factor(d0$immunosuppression_pred_clean),
  immunosuppression_model_missing_ind = gt0_or0(d0$immunosuppression_pred_missing_ind),
  hepaticfailure_model = as_binary_factor(d0$hepaticfailure_pred_clean),
  hepaticfailure_model_missing_ind = gt0_or0(d0$hepaticfailure_pred_missing_ind),
  metastaticcancer_model = as_binary_factor(d0$metastaticcancer_pred_clean),
  metastaticcancer_model_missing_ind = gt0_or0(d0$metastaticcancer_pred_missing_ind),
  gender_factor = factor_with_missing(d0[[gender_col]]),
  ethnicity_group = lump_small_levels(d0[[ethnicity_col]], min_n = 30L),
  unit_admit_source_group = unit_source_group(d0[[unit_admit_col]]),
  unit_type_group = unit_type_group(d0[[unit_type_col]]),
  hosp_window_volume_bucket_factor = factor_with_missing(d0$hosp_main_three_window_bucket),
  hosp_zero_abx_reporting_ind = gt0_or0(d0$hosp_zero_abx_reporting_ind),
  stringsAsFactors = FALSE
)

rare_binary <- c("cirrhosis_model", "immunosuppression_model", "hepaticfailure_model", "metastaticcancer_model")
for (v in rare_binary) {
  y[[paste0(v, "_raw_for_impute")]] <- as_int01(y[[v]])
  y[[paste0(v, "_was_missing")]] <- gt0_or0(y[[paste0(v, "_missing_ind")]])
  y[[v]] <- factor(ifelse(is.na(as_int01(y[[v]])), 0L, as_int01(y[[v]])), levels = c(0L, 1L))
}

factor_cols <- vapply(y, is.factor, logical(1))
y[factor_cols] <- lapply(y[factor_cols], droplevels)

actual_targets <- c("apache_score_iva_model", "vent_model", "intubated_model", "dialysis_model", "diabetes_model")
mice_vars <- c(
  "age_model", actual_targets,
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "sens_late_12_72h", "death_30d_inhosp"
)
mice_dat <- y[, mice_vars, drop = FALSE]

method <- mice::make.method(mice_dat)
method[] <- ""
if (sum(is.na(mice_dat$apache_score_iva_model)) > 0) method["apache_score_iva_model"] <- "pmm"
for (v in c("vent_model", "intubated_model", "dialysis_model", "diabetes_model")) {
  if (sum(is.na(mice_dat[[v]])) > 0) method[v] <- "logreg"
}

pred <- mice::make.predictorMatrix(mice_dat)
pred[,] <- 0
predictor_cols <- setdiff(names(mice_dat), actual_targets)
predictor_cols <- setdiff(predictor_cols, character(0))
for (target in names(method)[method != ""]) {
  pred[target, setdiff(names(mice_dat), target)] <- 1
  pred[target, c("sens_late_12_72h", "death_30d_inhosp")] <- 1
}
if ("apache_score_iva_model" %in% rownames(pred) && "gender_factor" %in% colnames(pred)) {
  pred["apache_score_iva_model", "gender_factor"] <- 0
}

missingness <- data.frame(
  variable = names(mice_dat),
  class = vapply(mice_dat, function(z) paste(class(z), collapse = "/"), character(1)),
  missing_n = vapply(mice_dat, function(z) sum(is.na(z)), integer(1)),
  missing_pct = round(100 * vapply(mice_dat, function(z) mean(is.na(z)), numeric(1)), 2),
  method = unname(method[names(mice_dat)]),
  stringsAsFactors = FALSE
)
write.csv(missingness, out_missingness, row.names = FALSE, na = "")
write.csv(data.frame(variable = names(method), method = unname(method)), out_method, row.names = FALSE, na = "")
write.csv(y, out_input, row.names = FALSE, na = "")

set.seed(seed)
imp <- mice::mice(
  mice_dat,
  m = m_imp,
  maxit = maxit,
  method = method,
  predictorMatrix = pred,
  seed = seed,
  printFlag = FALSE
)

completed_list <- vector("list", m_imp)
for (ii in seq_len(m_imp)) {
  comp <- mice::complete(imp, action = ii)
  out <- y
  out$.imp <- ii
  for (v in actual_targets) {
    out[[v]] <- completed_vector(comp[[v]])
  }
  for (v in rare_binary) {
    out[[v]] <- as_int01(out[[v]])
  }
  completed_list[[ii]] <- out
}
model_ready <- do.call(rbind, completed_list)
model_ready$.imp <- as.integer(model_ready$.imp)

model_ready$gender_factor <- set_factor_ref(model_ready$gender_factor, preferred_ref = "Male")
model_ready$ethnicity_group <- set_factor_ref(model_ready$ethnicity_group, preferred_ref = "Caucasian")
model_ready$unit_admit_source_group <- set_factor_ref(model_ready$unit_admit_source_group)
model_ready$unit_type_group <- set_factor_ref(model_ready$unit_type_group)
model_ready$hosp_window_volume_bucket_factor <- set_factor_ref(model_ready$hosp_window_volume_bucket_factor, preferred_ref = ">30")
model_ready$hospitalid <- factor(model_ready$hospitalid)
model_ready$vent_model <- factor(as_int01(model_ready$vent_model), levels = c(0, 1))
model_ready$intubated_model <- factor(as_int01(model_ready$intubated_model), levels = c(0, 1))
model_ready$dialysis_model <- factor(as_int01(model_ready$dialysis_model), levels = c(0, 1))
model_ready$diabetes_model <- factor(as_int01(model_ready$diabetes_model), levels = c(0, 1))
for (v in rare_binary) model_ready[[v]] <- factor(as_int01(model_ready[[v]]), levels = c(0, 1))

write.csv(model_ready, out_model_ready, row.names = FALSE, na = "")

qc_by_imp <- do.call(rbind, lapply(sort(unique(model_ready$.imp)), function(ii) {
  z <- model_ready[model_ready$.imp == ii, , drop = FALSE]
  data.frame(
    imp = ii,
    rows_n = nrow(z),
    distinct_stays = length(unique(z$patientunitstayid)),
    n_hosp = length(unique(z$hospitalid)),
    death_n = sum(z$death_30d_inhosp == 1, na.rm = TRUE),
    death_pct = round(100 * mean(z$death_30d_inhosp == 1, na.rm = TRUE), 2),
    early_0_3h_n = sum(z$sens_late_12_72h == 0, na.rm = TRUE),
    late_12_72h_n = sum(z$sens_late_12_72h == 1, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))

base_terms <- c(
  "sens_late_12_72h",
  "age_model",
  "apache_score_iva_model",
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "diabetes_model",
  "apache_score_iva_model_missing_ind",
  "diabetes_model_missing_ind"
)
support_terms <- c(base_terms, "vent_model", "intubated_model", "dialysis_model")
rare_terms <- c(base_terms, rare_binary, "cirrhosis_model_missing_ind")

term_sets <- list(
  sensitivity_crude_glm = c("sens_late_12_72h"),
  sensitivity_adjusted_glm = base_terms,
  sensitivity_support_expanded_glm = support_terms,
  sensitivity_rare_comorbidity_glm = rare_terms
)
if (run_glmm) term_sets$sensitivity_hospital_random_intercept_glmm <- base_terms

formula_plan <- do.call(rbind, lapply(names(term_sets), function(model_id) {
  requested <- term_sets[[model_id]]
  dropped <- requested[!vapply(requested, function(v) has_variation(model_ready[[v]]), logical(1))]
  kept <- setdiff(requested, dropped)
  formula_txt <- paste(deparse(make_formula(
    "death_30d_inhosp",
    kept,
    random_intercept = identical(model_id, "sensitivity_hospital_random_intercept_glmm")
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

all_terms <- list()
all_diag <- list()
for (model_id in names(term_sets)) {
  requested <- term_sets[[model_id]]
  kept <- requested[vapply(requested, function(v) has_variation(model_ready[[v]]), logical(1))]
  is_glmm <- identical(model_id, "sensitivity_hospital_random_intercept_glmm")
  f <- make_formula("death_30d_inhosp", kept, random_intercept = is_glmm)
  fit_obj <- fit_model_by_imp(model_ready, f, model_id, if (is_glmm) "glmer" else "glm")
  all_terms[[model_id]] <- pool_fixed_effects(fit_obj$fits, model_id)
  all_diag[[model_id]] <- fit_obj$diagnostics
}

terms_out <- do.call(rbind, all_terms)
terms_out$is_exposure_contrast <- terms_out$term == "sens_late_12_72h"
fit_diag <- do.call(rbind, all_diag)
write.csv(terms_out, out_terms, row.names = FALSE, na = "")
write.csv(fit_diag, out_fit_diag, row.names = FALSE, na = "")

exposure <- terms_out[terms_out$is_exposure_contrast, , drop = FALSE]
exposure$contrast <- ">12-72h vs 0-3h"
write.csv(exposure, out_exposure, row.names = FALSE, na = "")

logged_events <- if (is.null(imp$loggedEvents)) data.frame() else imp$loggedEvents
logged_events_n <- nrow(logged_events)

report_lines <- c(
  "eICU formal_v1 >12-72h sensitivity report",
  paste("input:", input_file),
  paste("m:", m_imp),
  paste("maxit:", maxit),
  paste("seed:", seed),
  "",
  "analysis set by completed imputation:",
  capture.output(print(qc_by_imp, row.names = FALSE)),
  "",
  "MICE missingness:",
  capture.output(print(missingness, row.names = FALSE)),
  "",
  paste("MICE logged events:", logged_events_n),
  if (logged_events_n > 0) capture.output(print(logged_events, row.names = FALSE)) else "none",
  "",
  "formula plan:",
  capture.output(print(formula_plan[, c("model_id", "dropped_nonvarying_terms", "formula")], row.names = FALSE)),
  "",
  "exposure summary:",
  capture.output(print(exposure[, c("model_id", "contrast", "or", "or_low", "or_high", "p_value", "m_used")], row.names = FALSE)),
  "",
  "fit diagnostics:",
  capture.output(print(fit_diag, row.names = FALSE)),
  "",
  "outputs:",
  out_input,
  out_model_ready,
  out_terms,
  out_exposure,
  out_fit_diag
)
writeLines(report_lines, out_report)

message("Done. Exposure summary:")
print(exposure[, c("model_id", "contrast", "or", "or_low", "or_high", "p_value", "m_used")], row.names = FALSE)
message("Report: ", out_report)
