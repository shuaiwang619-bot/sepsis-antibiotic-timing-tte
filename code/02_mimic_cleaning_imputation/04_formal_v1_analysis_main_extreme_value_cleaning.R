# formal_v1 analysis_main extreme-value cleaning
# Scope: wide-table numeric plausibility checks only.
# This script does not impute values and does not delete patient rows.
# It preserves original columns unchanged, saves numeric raw values in
# *_numeric_raw, converts implausible or parse-invalid values to NA only in
# *_clean columns, and flags them with *_extreme_or_invalid indicators.
# Later imputation scripts may impute these *_clean NA values when the variable
# is eligible for modeling.

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

default_input <- file.path(base_dir, "formal_v1_analysis_main_categorical_encoded.csv")
if (!file.exists(default_input)) {
  default_input <- file.path(base_dir, "formal_v1_analysis_main.csv")
}

input_file <- if (length(args) >= 2) args[2] else default_input
out_cleaned <- if (length(args) >= 3) args[3] else file.path(base_dir, "formal_v1_analysis_main_extreme_cleaned.csv")
out_summary <- file.path(base_dir, "formal_v1_analysis_main_extreme_value_summary.csv")
out_rows <- file.path(base_dir, "formal_v1_analysis_main_extreme_value_flagged_rows.csv")
out_rules <- file.path(base_dir, "formal_v1_analysis_main_extreme_value_rules.csv")
out_report <- file.path(base_dir, "formal_v1_analysis_main_extreme_value_report.txt")

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

empty_summary <- function() {
  data.frame(
    variable = character(),
    role = character(),
    rule_type = character(),
    lower = numeric(),
    upper = numeric(),
    allowed_values = character(),
    n = integer(),
    missing_before = integer(),
    parse_invalid_n = integer(),
    range_or_allowed_invalid_n = integer(),
    total_flagged_n = integer(),
    missing_after_clean = integer(),
    min_before = numeric(),
    max_before = numeric(),
    min_after_clean = numeric(),
    max_after_clean = numeric(),
    clean_column = character(),
    flag_column = character(),
    action = character(),
    later_imputation = character(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

summarize_values <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0) return(c(min = NA_real_, max = NA_real_))
  c(min = min(z), max = max(z))
}

apply_range_rule <- function(dat, var, lower, upper, role, action, later_imputation, note) {
  if (!var %in% names(dat)) return(list(dat = dat, summary = empty_summary()))

  raw <- dat[[var]]
  miss <- is_miss(raw)
  num <- to_num(raw)
  parse_invalid <- !miss & is.na(num)
  range_invalid <- !is.na(num) & (num < lower | num > upper)
  flagged <- parse_invalid | range_invalid
  clean <- num
  clean[flagged] <- NA_real_

  raw_col <- paste0(var, "_numeric_raw")
  clean_col <- paste0(var, "_clean")
  flag_col <- paste0(var, "_extreme_or_invalid")
  parse_col <- paste0(var, "_parse_invalid")

  dat[[raw_col]] <- num
  dat[[clean_col]] <- clean
  dat[[flag_col]] <- as.integer(flagged)
  dat[[parse_col]] <- as.integer(parse_invalid)

  before_mm <- summarize_values(num)
  after_mm <- summarize_values(clean)

  summary <- data.frame(
    variable = var,
    role = role,
    rule_type = "range",
    lower = lower,
    upper = upper,
    allowed_values = "",
    n = length(raw),
    missing_before = sum(miss),
    parse_invalid_n = sum(parse_invalid),
    range_or_allowed_invalid_n = sum(range_invalid),
    total_flagged_n = sum(flagged),
    missing_after_clean = sum(is.na(clean)),
    min_before = before_mm["min"],
    max_before = before_mm["max"],
    min_after_clean = after_mm["min"],
    max_after_clean = after_mm["max"],
    clean_column = clean_col,
    flag_column = flag_col,
    action = action,
    later_imputation = later_imputation,
    note = note,
    stringsAsFactors = FALSE
  )

  list(dat = dat, summary = summary)
}

apply_allowed_rule <- function(dat, var, allowed, role, action, later_imputation, note) {
  if (!var %in% names(dat)) return(list(dat = dat, summary = empty_summary()))

  raw <- dat[[var]]
  miss <- is_miss(raw)
  num <- to_num(raw)
  parse_invalid <- !miss & is.na(num)
  allowed_invalid <- !is.na(num) & !(num %in% allowed)
  flagged <- parse_invalid | allowed_invalid
  clean <- num
  clean[flagged] <- NA_real_

  raw_col <- paste0(var, "_numeric_raw")
  clean_col <- paste0(var, "_clean")
  flag_col <- paste0(var, "_extreme_or_invalid")
  parse_col <- paste0(var, "_parse_invalid")

  dat[[raw_col]] <- num
  dat[[clean_col]] <- clean
  dat[[flag_col]] <- as.integer(flagged)
  dat[[parse_col]] <- as.integer(parse_invalid)

  before_mm <- summarize_values(num)
  after_mm <- summarize_values(clean)

  summary <- data.frame(
    variable = var,
    role = role,
    rule_type = "allowed_values",
    lower = NA_real_,
    upper = NA_real_,
    allowed_values = paste(allowed, collapse = "|"),
    n = length(raw),
    missing_before = sum(miss),
    parse_invalid_n = sum(parse_invalid),
    range_or_allowed_invalid_n = sum(allowed_invalid),
    total_flagged_n = sum(flagged),
    missing_after_clean = sum(is.na(clean)),
    min_before = before_mm["min"],
    max_before = before_mm["max"],
    min_after_clean = after_mm["min"],
    max_after_clean = after_mm["max"],
    clean_column = clean_col,
    flag_column = flag_col,
    action = action,
    later_imputation = later_imputation,
    note = note,
    stringsAsFactors = FALSE
  )

  list(dat = dat, summary = summary)
}

x <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
y <- x
summaries <- list()

add_range <- function(var, lower, upper, role, action, later_imputation, note) {
  res <- apply_range_rule(y, var, lower, upper, role, action, later_imputation, note)
  y <<- res$dat
  summaries[[length(summaries) + 1L]] <<- res$summary
}

add_allowed <- function(var, allowed, role, action, later_imputation, note) {
  res <- apply_allowed_rule(y, var, allowed, role, action, later_imputation, note)
  y <<- res$dat
  summaries[[length(summaries) + 1L]] <<- res$summary
}

# Main model candidates and baseline severity variables.
add_range("age_at_icu", 18, 120, "main_model_candidate", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "yes_if_used_in_model", "adult ICU cohort; values above 120 are treated as implausible")
add_range("charlson_comorbidity_index", 0, 40, "main_model_candidate", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "yes_if_used_in_model", "Charlson score kept numeric; range is permissive")
add_range("sofa_total_t0", 0, 24, "main_model_core", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "yes_with_missing_indicator", "SOFA total valid range")
add_range("meanbp_min_t0", 5, 200, "sensitivity_only", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "yes_for_sensitivity_only", "not forced into main model; lower bound is physiologic floor to avoid erasing real severe shock values")

# SOFA components and high-missing baseline variables.
for (v in c(
  "respiration_24hours_t0",
  "coagulation_24hours_t0",
  "liver_24hours_t0",
  "cardiovascular_24hours_t0",
  "cns_24hours_t0",
  "renal_24hours_t0"
)) {
  add_range(v, 0, 4, "extension_or_sensitivity", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "yes_for_extension_only", "SOFA component valid range")
}
add_range("gcs_min_t0", 3, 15, "not_main_high_missing", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "not_main_model; optional_sensitivity_only", "GCS high missingness; descriptive/sensitivity only")
add_range("uo_24hr_t0", 0, 20000, "not_main_high_missing", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "not_main_model; optional_sensitivity_only", "urine output kept broad; high missingness means not main model")

# Post-baseline or outcome-related variables: diagnostic cleaning only.
add_range("sofa_peak_72h", 0, 24, "post_baseline_auxiliary", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "no_main_model_imputation", "post-baseline peak; not a baseline confounder")
add_allowed("organ_deterioration_sofa", c(0, 1), "post_baseline_auxiliary", "convert non-0/1 or parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "no_main_model_imputation", "binary deterioration flag")
add_range("icu_los_from_t0_days", 0, 365, "descriptive_or_outcome_related", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "no_main_model_imputation", "LOS is not a baseline confounder")
add_range("hosp_los_from_t0_days", 0, 365, "descriptive_or_outcome_related", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "no_main_model_imputation", "LOS is not a baseline confounder")
add_range("fu_time_30d_days", 0, 30, "outcome_followup", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "no_outcome_imputation_without_review", "30-day follow-up time should stay within 0-30 days")

# Exposure-definition diagnostics only; never use as ordinary confounders.
add_range("abx_delay_hours", -72, 72, "exposure_definition_not_confounder", "convert implausible/parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "do_not_impute_exposure_without_review", "antibiotic delay definition field; negative values may indicate pre-t0 timing and should not be auto-erased")
add_allowed("abx_window_num", c(0, 1, 2, 3, 4), "exposure_definition_not_confounder", "convert unexpected codes to NA in *_clean; preserve original column and *_numeric_raw", "do_not_impute_exposure_without_review", "legacy exposure code; not continuous")
add_allowed("analyzable", c(0, 1), "cohort_role", "convert non-0/1 or parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", "no_imputation", "cohort indicator")

# Binary covariates and outcomes. Outcome-derived fields remain diagnostics only.
binary_vars <- c(
  "myocardial_infarct",
  "congestive_heart_failure",
  "peripheral_vascular_disease",
  "cerebrovascular_disease",
  "dementia",
  "chronic_pulmonary_disease",
  "rheumatic_disease",
  "peptic_ulcer_disease",
  "mild_liver_disease",
  "severe_liver_disease",
  "diabetes_without_cc",
  "diabetes_with_cc",
  "paraplegia",
  "renal_disease",
  "malignant_cancer",
  "metastatic_solid_tumor",
  "aids",
  "sex_num",
  "shock_t0_b_num",
  "weekend_admission_num",
  "admission_emergency",
  "death_30d",
  "death_icu",
  "death_hospital",
  "death_in_hospital",
  "death_out_hospital",
  "death_no_record"
)
for (v in binary_vars) {
  role <- if (grepl("^death", v)) "outcome_or_outcome_derived_diagnostics" else "binary_covariate_or_indicator"
  later_imputation <- if (grepl("^death", v)) "no_outcome_imputation_without_review" else "yes_if_used_in_model"
  add_allowed(v, c(0, 1), role, "convert non-0/1 or parse-invalid to NA in *_clean; preserve original column and *_numeric_raw", later_imputation, "binary value check")
}

summary_df <- do.call(rbind, summaries)
summary_df <- summary_df[summary_df$variable != "", , drop = FALSE]

flag_cols <- summary_df$flag_column[summary_df$flag_column %in% names(y)]
if (length(flag_cols) > 0) {
  y$any_extreme_or_invalid <- as.integer(rowSums(y[, flag_cols, drop = FALSE], na.rm = TRUE) > 0)
} else {
  y$any_extreme_or_invalid <- 0L
}

id_cols <- intersect(c("stay_id", "subject_id", "hadm_id", "abx_window", "ccw_role"), names(y))
flagged_rows <- y[y$any_extreme_or_invalid == 1L, c(id_cols, flag_cols), drop = FALSE]

rules_df <- summary_df[, c(
  "variable",
  "role",
  "rule_type",
  "lower",
  "upper",
  "allowed_values",
  "action",
  "later_imputation",
  "note",
  "clean_column",
  "flag_column"
), drop = FALSE]

write.csv(y, out_cleaned, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summary_df, out_summary, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(flagged_rows, out_rows, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(rules_df, out_rules, row.names = FALSE, fileEncoding = "UTF-8")

sink(out_report)
cat("formal_v1 analysis_main extreme-value cleaning report\n")
cat("input:", input_file, "\n")
cat("rows:", nrow(y), "\n")
cat("output_cleaned:", out_cleaned, "\n")
cat("output_summary:", out_summary, "\n")
cat("output_flagged_rows:", out_rows, "\n")
cat("output_rules:", out_rules, "\n\n")

cat("variables_checked:", nrow(summary_df), "\n")
cat("rows_with_any_extreme_or_invalid:", sum(y$any_extreme_or_invalid == 1L, na.rm = TRUE), "\n\n")

cat("flagged_by_variable:\n")
print(summary_df[summary_df$total_flagged_n > 0, c(
  "variable",
  "role",
  "missing_before",
  "parse_invalid_n",
  "range_or_allowed_invalid_n",
  "total_flagged_n",
  "min_before",
  "max_before",
  "min_after_clean",
  "max_after_clean"
), drop = FALSE], row.names = FALSE)

cat("\nmain_model_notes:\n")
cat("- Use *_clean columns for variables after this step; keep raw columns for audit.\n")
cat("- Extreme/invalid values are converted to NA only in *_clean columns; original columns remain unchanged.\n")
cat("- Later imputation may impute these *_clean NA values only according to variable role and later_imputation policy.\n")
cat("- Do not force meanbp_min_t0 into the main model; use meanbp_min_t0_clean and its flag only for sensitivity/auxiliary models.\n")
cat("- Do not use abx_delay_hours, abx_window_num, death_site-derived variables, or LOS variables as ordinary baseline confounders.\n")
cat("- This script does not impute; missingness handling comes in a later script.\n")
sink()

cat("Wrote:\n")
cat(out_cleaned, "\n")
cat(out_summary, "\n")
cat(out_rows, "\n")
cat(out_rules, "\n")
cat(out_report, "\n")
