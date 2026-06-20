# formal_v1 analysis_main MICE imputation
# Scope: model-ready baseline covariates only.
# Default: m = 5, maxit = 20, seed = 2025.
# This script does not impute exposure, outcome, ID, high-missing GCS/urine
# output, LOS, death-site-derived variables, or antibiotic delay fields.

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
input_file <- if (length(args) >= 2) args[2] else file.path(base_dir, "formal_v1_analysis_main_extreme_cleaned.csv")
m_imp <- if (length(args) >= 3) as.integer(args[3]) else 5L
maxit <- if (length(args) >= 4) as.integer(args[4]) else 20L
seed <- if (length(args) >= 5) as.integer(args[5]) else 2025L

out_long <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_model_ready_long.csv")
out_imputed_values <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_imputed_values.csv")
out_summary <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_summary.csv")
out_report <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_report.txt")
out_mids <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_mids.rds")

if (!requireNamespace("mice", quietly = TRUE)) {
  stop("Package 'mice' is required. Install it before running this script.", call. = FALSE)
}

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

first_existing <- function(dat, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(dat)]
  if (length(hit) > 0) return(hit[1])
  if (required) stop("Missing required column; tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  NA_character_
}

as_factor_clean <- function(x) {
  out <- as.factor(as.character(x))
  droplevels(out)
}

x <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c(
  "stay_id",
  "subject_id",
  "hadm_id",
  "abx_window_factor",
  "ccw_role_factor",
  "death_30d",
  "fu_time_30d_days",
  "sex_factor",
  "race_group",
  "admission_type_group",
  "shock_t0_b_factor"
)
missing_required <- setdiff(required_cols, names(x))
if (length(missing_required) > 0) {
  stop("Missing required column(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
}

age_col <- first_existing(x, c("age_at_icu_clean", "age_at_icu"))
charlson_col <- first_existing(x, c("charlson_comorbidity_index_clean", "charlson_comorbidity_index"))
sofa_col <- first_existing(x, c("sofa_total_t0_clean", "sofa_total_t0"))
meanbp_col <- first_existing(x, c("meanbp_min_t0_clean", "meanbp_min_t0"), required = FALSE)
shock_num_col <- first_existing(x, c("shock_t0_b_num_clean", "shock_t0_b_num"), required = FALSE)

y <- data.frame(
  stay_id = x$stay_id,
  subject_id = x$subject_id,
  hadm_id = x$hadm_id,
  abx_window_factor = as_factor_clean(x$abx_window_factor),
  ccw_role_factor = as_factor_clean(x$ccw_role_factor),
  death_30d = to_num(x$death_30d),
  fu_time_30d_days = to_num(x$fu_time_30d_days),
  age_at_icu_model = to_num(x[[age_col]]),
  sex_factor = as_factor_clean(x$sex_factor),
  race_group = as_factor_clean(x$race_group),
  admission_type_group = as_factor_clean(x$admission_type_group),
  charlson_comorbidity_index_model = to_num(x[[charlson_col]]),
  sofa_total_t0_model = to_num(x[[sofa_col]]),
  shock_t0_b_factor = as_factor_clean(x$shock_t0_b_factor),
  stringsAsFactors = FALSE
)

y$sofa_total_t0_missing <- as.integer(is.na(y$sofa_total_t0_model))

if (!is.na(meanbp_col)) {
  y$meanbp_min_t0_sens <- to_num(x[[meanbp_col]])
  y$meanbp_min_t0_missing <- as.integer(is.na(y$meanbp_min_t0_sens))
  y$meanbp_min_t0_extreme_or_invalid <- if ("meanbp_min_t0_extreme_or_invalid" %in% names(x)) {
    to_num(x$meanbp_min_t0_extreme_or_invalid)
  } else {
    0
  }
} else {
  y$meanbp_min_t0_sens <- NA_real_
  y$meanbp_min_t0_missing <- 1L
  y$meanbp_min_t0_extreme_or_invalid <- 0
}

if (!is.na(shock_num_col)) {
  y$shock_t0_b_num_model <- to_num(x[[shock_num_col]])
} else {
  y$shock_t0_b_num_model <- ifelse(y$shock_t0_b_factor == "shock", 1L, 0L)
}

# Restrict imputation targets to planned model variables.
# - Main model imputation target: sofa_total_t0_model, with missing indicator retained.
# - Sensitivity target: meanbp_min_t0_sens, not used in the main model by default.
# - Outcomes/exposures are included as predictors but are never imputed.
mice_dat <- y[, c(
  "age_at_icu_model",
  "sex_factor",
  "race_group",
  "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_model",
  "sofa_total_t0_missing",
  "shock_t0_b_factor",
  "meanbp_min_t0_sens",
  "meanbp_min_t0_missing",
  "meanbp_min_t0_extreme_or_invalid",
  "abx_window_factor",
  "ccw_role_factor",
  "death_30d",
  "fu_time_30d_days"
)]

missing_before <- vapply(mice_dat, function(v) sum(is.na(v)), integer(1))

meth <- mice::make.method(mice_dat)
meth[] <- ""
if (missing_before["sofa_total_t0_model"] > 0) meth["sofa_total_t0_model"] <- "pmm"
if (missing_before["meanbp_min_t0_sens"] > 0) meth["meanbp_min_t0_sens"] <- "pmm"

pred <- mice::make.predictorMatrix(mice_dat)
pred[,] <- 0

predictor_cols <- c(
  "age_at_icu_model",
  "sex_factor",
  "race_group",
  "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_model",
  "sofa_total_t0_missing",
  "shock_t0_b_factor",
  "meanbp_min_t0_sens",
  "meanbp_min_t0_missing",
  "meanbp_min_t0_extreme_or_invalid",
  "abx_window_factor",
  "ccw_role_factor",
  "death_30d",
  "fu_time_30d_days"
)

target_vars <- names(meth)[meth != ""]
for (target in target_vars) {
  pred[target, setdiff(predictor_cols, target)] <- 1
}

set.seed(seed)
imp <- mice::mice(
  mice_dat,
  m = m_imp,
  maxit = maxit,
  method = meth,
  predictorMatrix = pred,
  seed = seed,
  printFlag = TRUE
)

base_out <- y[, c(
  "stay_id",
  "subject_id",
  "hadm_id",
  "abx_window_factor",
  "ccw_role_factor",
  "death_30d",
  "fu_time_30d_days",
  "age_at_icu_model",
  "sex_factor",
  "race_group",
  "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing",
  "shock_t0_b_factor",
  "shock_t0_b_num_model",
  "meanbp_min_t0_missing",
  "meanbp_min_t0_extreme_or_invalid"
)]
base_out$sofa_total_t0_raw_for_impute <- y$sofa_total_t0_model
base_out$meanbp_min_t0_raw_for_impute <- y$meanbp_min_t0_sens

make_completed_out <- function(i) {
  comp <- if (i == 0L) mice_dat else mice::complete(imp, action = i)
  out <- base_out
  out$.imp <- i
  out$sofa_total_t0_imp <- comp$sofa_total_t0_model
  out$meanbp_min_t0_imp_sens <- comp$meanbp_min_t0_sens
  out <- out[, c(
    ".imp",
    setdiff(names(out), ".imp")
  )]
  out
}

completed_list <- lapply(0:m_imp, make_completed_out)
completed_long <- do.call(rbind, completed_list)
write.csv(completed_long, out_long, row.names = FALSE, fileEncoding = "UTF-8")

imputed_rows <- list()
for (i in seq_len(m_imp)) {
  comp <- mice::complete(imp, action = i)
  for (v in target_vars) {
    idx <- which(is.na(mice_dat[[v]]))
    if (length(idx) == 0) next
    imputed_rows[[length(imputed_rows) + 1L]] <- data.frame(
      .imp = i,
      stay_id = y$stay_id[idx],
      variable = v,
      imputed_value = comp[[v]][idx],
      stringsAsFactors = FALSE
    )
  }
}
imputed_values <- if (length(imputed_rows) > 0) do.call(rbind, imputed_rows) else data.frame()
write.csv(imputed_values, out_imputed_values, row.names = FALSE, fileEncoding = "UTF-8")

summary_rows <- list()
for (v in target_vars) {
  original <- mice_dat[[v]]
  completed_values <- unlist(lapply(seq_len(m_imp), function(i) mice::complete(imp, action = i)[[v]]))
  imputed_only <- imputed_values$imputed_value[imputed_values$variable == v]
  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    variable = v,
    method = meth[v],
    m = m_imp,
    maxit = maxit,
    seed = seed,
    missing_before = sum(is.na(original)),
    missing_after_each_completed_dataset = 0L,
    original_min = min(original, na.rm = TRUE),
    original_median = median(original, na.rm = TRUE),
    original_max = max(original, na.rm = TRUE),
    imputed_min = if (length(imputed_only) > 0) min(imputed_only, na.rm = TRUE) else NA_real_,
    imputed_median = if (length(imputed_only) > 0) median(imputed_only, na.rm = TRUE) else NA_real_,
    imputed_max = if (length(imputed_only) > 0) max(imputed_only, na.rm = TRUE) else NA_real_,
    completed_min = min(completed_values, na.rm = TRUE),
    completed_median = median(completed_values, na.rm = TRUE),
    completed_max = max(completed_values, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}
summary_df <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
write.csv(summary_df, out_summary, row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(imp, out_mids)

sink(out_report)
cat("formal_v1 analysis_main MICE imputation report\n")
cat("input:", input_file, "\n")
cat("rows:", nrow(y), "\n")
cat("m:", m_imp, "\n")
cat("maxit:", maxit, "\n")
cat("seed:", seed, "\n\n")

cat("target_variables:\n")
print(data.frame(variable = target_vars, method = meth[target_vars], missing_before = missing_before[target_vars]), row.names = FALSE)

cat("\nvariables_not_imputed_by_design:\n")
cat("exposure: abx_window_factor, abx_window_num, abx_delay_hours\n")
cat("outcome: death_30d, fu_time_30d_days\n")
cat("high_missing_not_main: gcs_min_t0, uo_24hr_t0\n")
cat("outcome_derived: death_site variables, LOS variables\n\n")

cat("predictor_matrix_nonzero_rows:\n")
for (target in target_vars) {
  cat(target, ":", paste(names(which(pred[target, ] != 0)), collapse = ", "), "\n")
}

cat("\nsummary:\n")
print(summary_df, row.names = FALSE)

cat("\noutputs:\n")
cat(out_long, "\n")
cat(out_imputed_values, "\n")
cat(out_summary, "\n")
cat(out_mids, "\n")
sink()

cat("Wrote:\n")
cat(out_long, "\n")
cat(out_imputed_values, "\n")
cat(out_summary, "\n")
cat(out_report, "\n")
cat(out_mids, "\n")
