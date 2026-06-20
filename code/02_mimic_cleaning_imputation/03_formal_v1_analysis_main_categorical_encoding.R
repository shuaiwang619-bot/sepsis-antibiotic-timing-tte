# formal_v1 analysis_main categorical encoding
# Scope: categorical variables only. No continuous outlier handling or imputation.
# Updated after external Claude review: hard level checks, no silent factor NA,
# race missing goes to UNKNOWN, and legacy numeric codes are kept out of main modeling.

set.seed(2025)

args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else "."
input_file <- if (length(args) >= 2) args[2] else file.path(base_dir, "formal_v1_analysis_main.csv")

out_encoded <- if (length(args) >= 3) args[3] else file.path(base_dir, "formal_v1_analysis_main_categorical_encoded.csv")
out_counts <- file.path(base_dir, "formal_v1_analysis_main_categorical_counts.csv")
out_roles <- file.path(base_dir, "formal_v1_analysis_main_categorical_variable_roles.csv")
out_report <- file.path(base_dir, "formal_v1_analysis_main_categorical_encoding_report.txt")

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

to_clean_text <- function(z) {
  out <- trimws(as.character(z))
  out[is_miss(z)] <- NA_character_
  out
}

as_bool01 <- function(z) {
  zz <- tolower(trimws(as.character(z)))
  out <- ifelse(
    zz %in% c("t", "true", "1", "yes", "y"),
    1L,
    ifelse(zz %in% c("f", "false", "0", "no", "n"), 0L, NA_integer_)
  )
  out[is_miss(z)] <- NA_integer_
  out
}

assert_allowed_levels <- function(raw, allowed, variable_name) {
  raw_clean <- to_clean_text(raw)
  bad <- setdiff(unique(raw_clean[!is.na(raw_clean)]), allowed)
  if (length(bad) > 0) {
    stop(
      variable_name,
      " has unexpected value(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

make_factor_checked <- function(raw, allowed, variable_name, missing_level = NULL) {
  raw_clean <- to_clean_text(raw)
  assert_allowed_levels(raw_clean, allowed, variable_name)

  if (!is.null(missing_level)) {
    raw_clean[is.na(raw_clean)] <- missing_level
    levels_out <- unique(c(allowed, missing_level))
    out <- factor(raw_clean, levels = levels_out)
    stopifnot(sum(is.na(out)) == 0L)
  } else {
    out <- factor(raw_clean, levels = allowed)
    stopifnot(sum(is.na(out)) == sum(is_miss(raw)))
  }
  out
}

count_one <- function(dat, var) {
  tab <- table(dat[[var]], useNA = "ifany")
  data.frame(
    variable = var,
    value = names(tab),
    n = as.integer(tab),
    stringsAsFactors = FALSE
  )
}

x <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
y <- x

# 1) Sex ----------------------------------------------------------------------
gender_clean <- toupper(to_clean_text(y$gender))
y$sex_was_missing <- as.integer(is_miss(y$gender))
y$sex_num <- ifelse(gender_clean == "M", 1L, ifelse(gender_clean == "F", 0L, NA_integer_))
y$sex_factor <- factor(
  ifelse(is.na(gender_clean), "MISSING", gender_clean),
  levels = c("F", "M", "MISSING")
)
stopifnot(sum(is.na(y$sex_factor)) == 0L)

# 2) Shock --------------------------------------------------------------------
y$shock_t0_b_was_missing <- as.integer(is_miss(y$shock_t0_b))
y$shock_t0_b_num <- as_bool01(y$shock_t0_b)
y$shock_t0_b_factor <- factor(
  ifelse(is.na(y$shock_t0_b_num), "MISSING", ifelse(y$shock_t0_b_num == 1L, "shock", "no_shock")),
  levels = c("no_shock", "shock", "MISSING")
)
stopifnot(sum(is.na(y$shock_t0_b_factor)) == 0L)

# 3) Race ---------------------------------------------------------------------
# Legacy code is retained only for compatibility checks:
# WHITE=1, BLACK/AFRICAN=2, all other/unknown/missing=3.
race_text <- toupper(to_clean_text(y$race))
race_text_safe <- race_text
race_text_safe[is.na(race_text_safe)] <- ""

y$race_was_missing <- as.integer(is_miss(y$race))
y$race_num <- ifelse(grepl("WHITE", race_text_safe), 1L,
              ifelse(grepl("BLACK|AFRICAN", race_text_safe), 2L, 3L))
y$race_legacy_id <- y$race_num

y$race_group <- ifelse(is_miss(y$race), "UNKNOWN",
                ifelse(grepl("WHITE", race_text_safe), "WHITE",
                ifelse(grepl("BLACK|AFRICAN", race_text_safe), "BLACK",
                ifelse(grepl("ASIAN", race_text_safe), "ASIAN",
                ifelse(grepl("HISPANIC|LATINO|SOUTH AMERICAN", race_text_safe), "HISPANIC",
                ifelse(grepl("UNKNOWN|UNABLE|DECLINED", race_text_safe), "UNKNOWN", "OTHER"))))))
y$race_group <- factor(
  y$race_group,
  levels = c("WHITE", "BLACK", "ASIAN", "HISPANIC", "OTHER", "UNKNOWN")
)
stopifnot(sum(is.na(y$race_group)) == 0L)

# 4) Admission type -----------------------------------------------------------
adm_text <- toupper(to_clean_text(y$admission_type))
adm_text_safe <- adm_text
adm_text_safe[is.na(adm_text_safe)] <- ""

y$admission_type_was_missing <- as.integer(is_miss(y$admission_type))
y$admission_type_group <- ifelse(is.na(adm_text), "MISSING",
                          ifelse(grepl("EMER", adm_text_safe), "EMERGENCY",
                          ifelse(grepl("URGENT", adm_text_safe), "URGENT",
                          ifelse(grepl("ELECTIVE|SURGICAL", adm_text_safe), "ELECTIVE_SURGICAL",
                          ifelse(grepl("OBSERVATION", adm_text_safe), "OBSERVATION", "OTHER")))))
y$admission_type_group <- factor(
  y$admission_type_group,
  levels = c("EMERGENCY", "URGENT", "ELECTIVE_SURGICAL", "OBSERVATION", "OTHER", "MISSING")
)
stopifnot(sum(is.na(y$admission_type_group)) == 0L)
y$admission_emergency <- as.integer(y$admission_type_group == "EMERGENCY")
y$admission_emergency[y$admission_type_group == "MISSING"] <- NA_integer_

# 5) Weekend admission --------------------------------------------------------
y$weekend_admission_was_missing <- as.integer(is_miss(y$weekend_admission))
y$weekend_admission_num <- as_bool01(y$weekend_admission)

# 6) Antibiotic window exposure ----------------------------------------------
# Canonicalize only known display variants, then hard-stop on anything unknown.
abx_raw <- to_clean_text(y$abx_window)
abx_canonical <- abx_raw
abx_canonical <- gsub("\u2265", ">=", abx_canonical, fixed = TRUE)
y$abx_window_canonicalized <- as.integer(!is.na(abx_raw) & !is.na(abx_canonical) & abx_raw != abx_canonical)
y$abx_window_canonical <- abx_canonical

lvl_abx <- c(
  "0-3h",
  "3-6h",
  "6-12h",
  ">=12h",
  "no_iv_in_72h_after_t0",
  "pre_t0",
  "pre_t0_earlier_only"
)
assert_allowed_levels(y$abx_window_canonical, lvl_abx, "abx_window")
y$abx_window_factor <- factor(y$abx_window_canonical, levels = lvl_abx)
stopifnot(sum(is.na(y$abx_window_factor)) == sum(is_miss(y$abx_window)))

# 7) CCW role -----------------------------------------------------------------
lvl_ccw <- c("analyzable", "censor_noinit", "excluded_pretreated")
y$ccw_role_factor <- make_factor_checked(y$ccw_role, lvl_ccw, "ccw_role")

# 8) Death site: outcome-derived, diagnostics only ---------------------------
death_site_clean <- to_clean_text(y$death_site)
lvl_death_site <- c("alive_or_no_record", "in_hospital", "out_of_hospital")
assert_allowed_levels(death_site_clean, lvl_death_site, "death_site")

y$death_site_num <- ifelse(death_site_clean == "alive_or_no_record", 0L,
                    ifelse(death_site_clean == "in_hospital", 1L,
                    ifelse(death_site_clean == "out_of_hospital", 2L, NA_integer_)))
y$death_in_hospital <- ifelse(is.na(death_site_clean), NA_integer_, as.integer(death_site_clean == "in_hospital"))
y$death_out_hospital <- ifelse(is.na(death_site_clean), NA_integer_, as.integer(death_site_clean == "out_of_hospital"))
y$death_no_record <- ifelse(is.na(death_site_clean), NA_integer_, as.integer(death_site_clean == "alive_or_no_record"))

# 9) Variable role manifest ---------------------------------------------------
main_categorical_vars <- c(
  "sex_factor",
  "race_group",
  "admission_type_group",
  "shock_t0_b_factor"
)

legacy_numeric_vars <- c(
  "sex_num",
  "race_num",
  "race_legacy_id",
  "admission_emergency",
  "weekend_admission_num",
  "shock_t0_b_num"
)

exposure_or_role_vars <- c(
  "abx_window_factor",
  "ccw_role_factor"
)

outcome_derived_not_for_main_model <- c(
  "death_site_num",
  "death_in_hospital",
  "death_out_hospital",
  "death_no_record"
)

missing_indicator_vars <- c(
  "sex_was_missing",
  "shock_t0_b_was_missing",
  "race_was_missing",
  "admission_type_was_missing",
  "weekend_admission_was_missing"
)

roles <- rbind(
  data.frame(variable = main_categorical_vars, role = "main_model_factor", stringsAsFactors = FALSE),
  data.frame(variable = legacy_numeric_vars, role = "legacy_or_sensitivity_only", stringsAsFactors = FALSE),
  data.frame(variable = exposure_or_role_vars, role = "exposure_or_ccw_role_not_confounder", stringsAsFactors = FALSE),
  data.frame(variable = outcome_derived_not_for_main_model, role = "outcome_derived_diagnostics_only", stringsAsFactors = FALSE),
  data.frame(variable = missing_indicator_vars, role = "missingness_indicator", stringsAsFactors = FALSE)
)

# Guard against accidental formula shortcuts.
stopifnot(!"race_num" %in% main_categorical_vars)
stopifnot(!any(outcome_derived_not_for_main_model %in% main_categorical_vars))

# 10) Main analyzable exposure levels check ----------------------------------
dat_main <- droplevels(y[y$ccw_role == "analyzable", , drop = FALSE])
dat_main$abx_window_factor <- droplevels(dat_main$abx_window_factor)
dat_main$abx_window_factor <- stats::relevel(dat_main$abx_window_factor, ref = "0-3h")
stopifnot(nlevels(dat_main$abx_window_factor) == 4L)
stopifnot(identical(levels(dat_main$abx_window_factor), c("0-3h", "3-6h", "6-12h", ">=12h")))

# 11) Diagnostics and outputs -------------------------------------------------
count_vars <- c(
  "gender",
  "sex_factor",
  "shock_t0_b",
  "shock_t0_b_factor",
  "race",
  "race_group",
  "admission_type",
  "admission_type_group",
  "weekend_admission",
  "weekend_admission_num",
  "abx_window",
  "abx_window_factor",
  "ccw_role",
  "ccw_role_factor",
  "death_site",
  "death_site_num"
)
count_vars <- intersect(count_vars, names(y))
counts <- do.call(rbind, lapply(count_vars, function(v) count_one(y, v)))

write.csv(y, out_encoded, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(counts, out_counts, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(roles, out_roles, row.names = FALSE, fileEncoding = "UTF-8")

sink(out_report)
cat("formal_v1 analysis_main categorical encoding report\n")
cat("input:", input_file, "\n")
cat("rows:", nrow(y), "\n")
cat("output_encoded:", out_encoded, "\n")
cat("output_counts:", out_counts, "\n")
cat("output_roles:", out_roles, "\n\n")

cat("abx_window raw counts:\n")
print(table(y$abx_window, useNA = "ifany"))
cat("\nabx_window canonicalized rows:", sum(y$abx_window_canonicalized, na.rm = TRUE), "\n")
cat("\nabx_window_factor counts:\n")
print(table(y$abx_window_factor, useNA = "ifany"))

cat("\nccw_role_factor counts:\n")
print(table(y$ccw_role_factor, useNA = "ifany"))

cat("\nrace_group counts:\n")
print(table(y$race_group, useNA = "ifany"))

cat("\nadmission_type_group counts:\n")
print(table(y$admission_type_group, useNA = "ifany"))

cat("\nshock_t0_b_factor counts:\n")
print(table(y$shock_t0_b_factor, useNA = "ifany"))

cat("\nmain analyzable abx_window levels:\n")
print(levels(dat_main$abx_window_factor))
print(table(dat_main$abx_window_factor, useNA = "ifany"))

cat("\nmodeling note:\n")
cat("Use main_categorical_vars as factors. Do not use race_num as a continuous covariate.\n")
cat("Do not use death_site-derived variables in the main model.\n")
sink()

cat("Wrote:\n")
cat(out_encoded, "\n")
cat(out_counts, "\n")
cat(out_roles, "\n")
cat(out_report, "\n")
