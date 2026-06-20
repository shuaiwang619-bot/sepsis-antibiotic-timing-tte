# eICU formal_v1 step 04: sentinel and extreme-value cleaning
# MIMIC counterpart: 04_formal_v1_analysis_main_extreme_value_cleaning.R
#
# Scope:
#   Numeric/sentinel plausibility only. No imputation and no row deletion.
#
# Input:
#   outputs/wide_tables/eicu_formal_v1_analysis_main_categorical_encoded.csv
#
# Outputs:
#   outputs/wide_tables/eicu_formal_v1_analysis_main_extreme_cleaned.csv
#   outputs/wide_tables/eicu_formal_v1_arm2_mice_input.csv

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
  file.path(base_dir, "outputs", "wide_tables", "eicu_formal_v1_analysis_main_categorical_encoded.csv")
}

out_dir <- file.path(base_dir, "outputs", "wide_tables")
qc_dir <- file.path(base_dir, "outputs", "qc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

out_cleaned <- file.path(out_dir, "eicu_formal_v1_analysis_main_extreme_cleaned.csv")
out_arm2_mice_input <- file.path(out_dir, "eicu_formal_v1_arm2_mice_input.csv")
out_summary <- file.path(qc_dir, "eicu_formal_v1_step04_sentinel_extreme_summary.csv")
out_flagged_rows <- file.path(qc_dir, "eicu_formal_v1_step04_sentinel_extreme_flagged_rows.csv")
out_roles <- file.path(qc_dir, "eicu_formal_v1_step04_mice_candidate_variable_roles.csv")
out_arm2_candidate_audit <- file.path(qc_dir, "eicu_formal_v1_step04_arm2_mice_candidate_audit.csv")
out_report <- file.path(qc_dir, "eicu_formal_v1_step04_sentinel_extreme_report.txt")

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

first_existing <- function(dat, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(dat)]
  if (length(hit) > 0) return(hit[1])
  if (required) {
    stop("Missing required column; tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  NA_character_
}

has_any <- function(dat, candidates) {
  any(candidates %in% names(dat))
}

empty_summary <- function() {
  data.frame(
    variable = character(),
    source_column = character(),
    model_column = character(),
    missing_indicator_column = character(),
    role = character(),
    lower = numeric(),
    upper = numeric(),
    n = integer(),
    missing_before = integer(),
    sentinel_or_source_missing_n = integer(),
    range_invalid_n = integer(),
    missing_after = integer(),
    min_after = numeric(),
    p50_after = numeric(),
    max_after = numeric(),
    impute_eligible = logical(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

summarize_model_var <- function(dat, variable, source_col, model_col, miss_col,
                                lower = NA_real_, upper = NA_real_,
                                role, impute_eligible, note = "",
                                source_miss_candidates = character()) {
  value <- to_num(dat[[source_col]])
  source_miss_col <- first_existing(dat, source_miss_candidates, required = FALSE)
  if (!is.na(source_miss_col)) {
    source_missing <- to_num(dat[[source_miss_col]]) > 0
    source_missing[is.na(source_missing)] <- FALSE
    value[source_missing] <- NA_real_
  }
  missing_before <- sum(is.na(value))
  range_invalid <- rep(FALSE, length(value))
  if (!is.na(lower)) range_invalid <- range_invalid | (!is.na(value) & value < lower)
  if (!is.na(upper)) range_invalid <- range_invalid | (!is.na(value) & value > upper)
  model_value <- value
  model_value[range_invalid] <- NA_real_
  dat[[model_col]] <- model_value
  dat[[miss_col]] <- as.integer(is.na(model_value))

  nonmiss <- model_value[!is.na(model_value)]
  summary <- data.frame(
    variable = variable,
    source_column = source_col,
    model_column = model_col,
    missing_indicator_column = miss_col,
    role = role,
    lower = lower,
    upper = upper,
    n = length(model_value),
    missing_before = missing_before,
    sentinel_or_source_missing_n = missing_before,
    range_invalid_n = sum(range_invalid, na.rm = TRUE),
    missing_after = sum(is.na(model_value)),
    min_after = if (length(nonmiss) > 0) min(nonmiss) else NA_real_,
    p50_after = if (length(nonmiss) > 0) median(nonmiss) else NA_real_,
    max_after = if (length(nonmiss) > 0) max(nonmiss) else NA_real_,
    impute_eligible = impute_eligible,
    note = note,
    stringsAsFactors = FALSE
  )
  list(dat = dat, summary = summary, flagged = which(range_invalid))
}

summarize_binary_var <- function(dat, variable, source_col, model_col, miss_col,
                                 role, impute_eligible, note = "",
                                 source_miss_candidates = character()) {
  value <- to_num(dat[[source_col]])
  source_miss_col <- first_existing(dat, source_miss_candidates, required = FALSE)
  if (!is.na(source_miss_col)) {
    source_missing <- to_num(dat[[source_miss_col]]) > 0
    source_missing[is.na(source_missing)] <- FALSE
    value[source_missing] <- NA_real_
  }
  invalid <- !is.na(value) & !(value %in% c(0, 1))
  model_value <- value
  model_value[invalid] <- NA_real_
  dat[[model_col]] <- model_value
  dat[[miss_col]] <- as.integer(is.na(model_value))
  summary <- data.frame(
    variable = variable,
    source_column = source_col,
    model_column = model_col,
    missing_indicator_column = miss_col,
    role = role,
    lower = NA_real_,
    upper = NA_real_,
    n = length(model_value),
    missing_before = sum(is.na(value)),
    sentinel_or_source_missing_n = sum(is.na(value)),
    range_invalid_n = sum(invalid, na.rm = TRUE),
    missing_after = sum(is.na(model_value)),
    min_after = if (all(is.na(model_value))) NA_real_ else min(model_value, na.rm = TRUE),
    p50_after = if (all(is.na(model_value))) NA_real_ else median(model_value, na.rm = TRUE),
    max_after = if (all(is.na(model_value))) NA_real_ else max(model_value, na.rm = TRUE),
    impute_eligible = impute_eligible,
    note = note,
    stringsAsFactors = FALSE
  )
  list(dat = dat, summary = summary, flagged = which(invalid))
}

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
}

x <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
y <- x

required_cols <- c(
  "patientunitstayid",
  "death_30d_inhosp",
  "arm2_model_ind",
  "arm2_abx_timing",
  "arm2_late_3_12h"
)
missing_required <- setdiff(required_cols, names(y))
if (length(missing_required) > 0) {
  stop("Missing required column(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
}

summaries <- list()
flagged_rows <- list()

add_cont <- function(variable, candidates, model_col, miss_col, lower, upper,
                     role, impute_eligible, note = "",
                     source_miss_candidates = character()) {
  src <- first_existing(y, candidates, required = TRUE)
  res <- summarize_model_var(y, variable, src, model_col, miss_col, lower, upper, role, impute_eligible, note, source_miss_candidates)
  y <<- res$dat
  summaries[[length(summaries) + 1L]] <<- res$summary
  if (length(res$flagged) > 0) {
    flagged_rows[[length(flagged_rows) + 1L]] <<- data.frame(
      patientunitstayid = y$patientunitstayid[res$flagged],
      variable = variable,
      source_column = src,
      raw_value = x[[src]][res$flagged],
      reason = "range_invalid",
      stringsAsFactors = FALSE
    )
  }
}

add_binary <- function(variable, candidates, model_col, miss_col,
                       role, impute_eligible, note = "",
                       source_miss_candidates = character()) {
  src <- first_existing(y, candidates, required = TRUE)
  res <- summarize_binary_var(y, variable, src, model_col, miss_col, role, impute_eligible, note, source_miss_candidates)
  y <<- res$dat
  summaries[[length(summaries) + 1L]] <<- res$summary
  if (length(res$flagged) > 0) {
    flagged_rows[[length(flagged_rows) + 1L]] <<- data.frame(
      patientunitstayid = y$patientunitstayid[res$flagged],
      variable = variable,
      source_column = src,
      raw_value = x[[src]][res$flagged],
      reason = "not_0_or_1",
      stringsAsFactors = FALSE
    )
  }
}

# Main MICE-eligible continuous covariates. APACHE IVa score is the primary
# severity proxy; individual physiology components are retained for descriptive
# or sensitivity analyses only.
add_cont("age", c("age_num", "age_raw", "age_model"), "age_model", "age_model_missing_ind", 18, 120,
         "baseline_demographic", TRUE, "Adult first ICU cohort; no SQL imputation.",
         source_miss_candidates = c("age_missing_ind", "age_model_missing_ind"))
add_cont("apache_score_iva", c("apache_score_iva_clean", "apache_score_iva_raw_model", "apache_score_iva_model"), "apache_score_iva_model", "apache_score_iva_model_missing_ind", 0, 300,
         "severity_score_main_candidate", TRUE, "APACHE IVa score; -1 sentinel already converted to NA in SQL.",
         source_miss_candidates = c("apache_score_iva_missing_ind", "apache_score_iva_model_missing_ind"))

optional_cont_specs <- list(
  list("heartrate_apache", c("heartrate_apache_final", "heartrate_apache_clean", "heartrate_apache_model"), "heartrate_apache_model", "heartrate_apache_model_missing_ind", 20, 300, "baseline_physiology_sensitivity"),
  list("meanbp_apache", c("meanbp_apache_final", "meanbp_apache_clean", "meanbp_apache_model"), "meanbp_apache_model", "meanbp_apache_model_missing_ind", 20, 250, "baseline_physiology_sensitivity"),
  list("respiratoryrate_apache", c("respiratoryrate_apache_final", "respiratoryrate_apache_clean", "respiratoryrate_apache_model"), "respiratoryrate_apache_model", "respiratoryrate_apache_model_missing_ind", 1, 80, "baseline_physiology_sensitivity"),
  list("temperature_apache", c("temperature_apache_final", "temperature_apache_clean", "temperature_apache_model"), "temperature_apache_model", "temperature_apache_model_missing_ind", 25, 45, "baseline_physiology_sensitivity"),
  list("creatinine_apache", c("creatinine_apache_final", "creatinine_apache_clean", "creatinine_apache_model"), "creatinine_apache_model", "creatinine_apache_model_missing_ind", 0.1, 30, "baseline_lab_sensitivity"),
  list("wbc_apache", c("wbc_apache_final", "wbc_apache_clean", "wbc_apache_model"), "wbc_apache_model", "wbc_apache_model_missing_ind", 0.1, 500, "baseline_lab_sensitivity"),
  list("sodium_apache", c("sodium_apache_final", "sodium_apache_clean", "sodium_apache_model"), "sodium_apache_model", "sodium_apache_model_missing_ind", 90, 190, "baseline_lab_sensitivity"),
  list("glucose_apache", c("glucose_apache_final", "glucose_apache_clean", "glucose_apache_model"), "glucose_apache_model", "glucose_apache_model_missing_ind", 20, 1000, "baseline_lab_sensitivity")
)

for (spec in optional_cont_specs) {
  if (has_any(y, spec[[2]])) {
    add_cont(spec[[1]], spec[[2]], spec[[3]], spec[[4]], spec[[5]], spec[[6]],
             spec[[7]], FALSE,
             "APACHE score is the main severity proxy; raw physiology is descriptive/sensitivity only.")
  }
}

# High-missing variables retained for descriptive/sensitivity only.
for (spec in list(
  list("bilirubin_apache", c("bilirubin_apache_clean"), "bilirubin_apache_desc", "bilirubin_apache_desc_missing_ind", 0, 80),
  list("albumin_apache", c("albumin_apache_clean"), "albumin_apache_desc", "albumin_apache_desc_missing_ind", 0.5, 8),
  list("pao2_apache", c("pao2_apache_clean"), "pao2_apache_desc", "pao2_apache_desc_missing_ind", 20, 700),
  list("ph_apache", c("ph_apache_clean"), "ph_apache_desc", "ph_apache_desc_missing_ind", 6.5, 8.0)
)) {
  if (spec[[2]][1] %in% names(y)) {
    add_cont(spec[[1]], spec[[2]], spec[[3]], spec[[4]], spec[[5]], spec[[6]],
             "high_missing_descriptive_or_sensitivity", FALSE,
             "Not a main MICE target because missingness is high.")
  }
}

# MICE-eligible binary covariates. Frequent support/comorbidity variables can be
# imputed; rare comorbidity flags are retained as observed + missing indicators.
add_binary("vent", c("vent_apache_clean", "vent_model"), "vent_model", "vent_model_missing_ind",
           "baseline_support_candidate", TRUE, "Not SQL-imputed; MICE target if missing.",
           source_miss_candidates = c("vent_apache_missing_ind", "vent_model_missing_ind"))
add_binary("intubated", c("intubated_apache_clean", "intubated_model"), "intubated_model", "intubated_model_missing_ind",
           "baseline_support_candidate", TRUE, "Not SQL-imputed; MICE target if missing.",
           source_miss_candidates = c("intubated_apache_missing_ind", "intubated_model_missing_ind"))
add_binary("dialysis", c("dialysis_apache_clean", "dialysis_model"), "dialysis_model", "dialysis_model_missing_ind",
           "baseline_support_candidate", TRUE, "Not SQL-imputed; MICE target if missing.",
           source_miss_candidates = c("dialysis_apache_missing_ind", "dialysis_model_missing_ind"))
add_binary("diabetes", c("diabetes_pred_clean", "diabetes_model"), "diabetes_model", "diabetes_model_missing_ind",
           "baseline_comorbidity_candidate", TRUE, "Not SQL-imputed; MICE target if missing.",
           source_miss_candidates = c("diabetes_pred_missing_ind", "diabetes_model_missing_ind"))
add_binary("cirrhosis", c("cirrhosis_pred_clean", "cirrhosis_model"), "cirrhosis_model", "cirrhosis_model_missing_ind",
           "baseline_comorbidity_indicator_only", FALSE, "Rare flag; keep observed value and missing indicator rather than logreg MICE.",
           source_miss_candidates = c("cirrhosis_pred_missing_ind", "cirrhosis_model_missing_ind"))
add_binary("immunosuppression", c("immunosuppression_pred_clean", "immunosuppression_model"), "immunosuppression_model", "immunosuppression_model_missing_ind",
           "baseline_comorbidity_indicator_only", FALSE, "Rare flag; keep observed value and missing indicator rather than logreg MICE.",
           source_miss_candidates = c("immunosuppression_pred_missing_ind", "immunosuppression_model_missing_ind"))
add_binary("hepaticfailure", c("hepaticfailure_pred_clean", "hepaticfailure_model"), "hepaticfailure_model", "hepaticfailure_model_missing_ind",
           "baseline_comorbidity_indicator_only", FALSE, "Rare flag; keep observed value and missing indicator rather than logreg MICE.",
           source_miss_candidates = c("hepaticfailure_pred_missing_ind", "hepaticfailure_model_missing_ind"))
add_binary("metastaticcancer", c("metastaticcancer_pred_clean", "metastaticcancer_model"), "metastaticcancer_model", "metastaticcancer_model_missing_ind",
           "baseline_comorbidity_indicator_only", FALSE, "Rare flag; keep observed value and missing indicator rather than logreg MICE.",
           source_miss_candidates = c("metastaticcancer_pred_missing_ind", "metastaticcancer_model_missing_ind"))

summary_df <- do.call(rbind, summaries)
flagged_df <- if (length(flagged_rows) > 0) do.call(rbind, flagged_rows) else data.frame()

roles <- summary_df[, c(
  "variable",
  "source_column",
  "model_column",
  "missing_indicator_column",
  "role",
  "impute_eligible",
  "note"
)]

write.csv(y, out_cleaned, row.names = FALSE, fileEncoding = "UTF-8")

arm2 <- y[which(to_num(y$arm2_model_ind) == 1), , drop = FALSE]
write.csv(arm2, out_arm2_mice_input, row.names = FALSE, fileEncoding = "UTF-8")

arm2_candidate_audit <- do.call(rbind, lapply(seq_len(nrow(roles)), function(i) {
  model_col <- roles$model_column[i]
  values <- to_num(arm2[[model_col]])
  observed_unique <- sort(unique(values[!is.na(values)]))
  is_binary <- length(observed_unique) > 0 && all(observed_unique %in% c(0, 1))
  nonmissing_n <- sum(!is.na(values))
  data.frame(
    variable = roles$variable[i],
    model_column = model_col,
    type = ifelse(is_binary, "binary", "continuous"),
    role = roles$role[i],
    impute_eligible = roles$impute_eligible[i],
    n = nrow(arm2),
    missing_n = sum(is.na(values)),
    missing_pct = round(100 * sum(is.na(values)) / nrow(arm2), 2),
    positive_n = if (is_binary) sum(values == 1, na.rm = TRUE) else NA_integer_,
    positive_pct = if (is_binary && nonmissing_n > 0) round(100 * sum(values == 1, na.rm = TRUE) / nonmissing_n, 2) else NA_real_,
    note = roles$note[i],
    stringsAsFactors = FALSE
  )
}))

write.csv(summary_df, out_summary, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(flagged_df, out_flagged_rows, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(roles, out_roles, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(arm2_candidate_audit, out_arm2_candidate_audit, row.names = FALSE, fileEncoding = "UTF-8")

sink(out_report)
cat("eICU formal_v1 step 04 sentinel/extreme cleaning report\n")
cat("input:", input_file, "\n")
cat("rows:", nrow(y), "\n")
cat("distinct patientunitstayid:", length(unique(y$patientunitstayid)), "\n")
cat("arm2 rows for MICE:", nrow(arm2), "\n\n")

cat("summary:\n")
print(summary_df, row.names = FALSE)

cat("\noutputs:\n")
cat(out_cleaned, "\n")
cat(out_arm2_mice_input, "\n")
cat(out_summary, "\n")
cat(out_flagged_rows, "\n")
cat(out_roles, "\n")
cat(out_arm2_candidate_audit, "\n")
sink()

cat("Wrote:\n")
cat(out_cleaned, "\n")
cat(out_arm2_mice_input, "\n")
cat(out_summary, "\n")
cat(out_flagged_rows, "\n")
cat(out_roles, "\n")
cat(out_arm2_candidate_audit, "\n")
cat(out_report, "\n")
