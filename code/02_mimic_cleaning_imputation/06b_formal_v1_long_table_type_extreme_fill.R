# formal_v1 long-table type conversion, extreme-value cleaning, and simple fills
# Scope: time-varying CCW/IPCW input only.
# This script preserves the database-exported original columns unchanged.
# It creates *_clean, *_missing_model, *_extreme_or_invalid, and *_fill columns.
# It does not perform MICE on the long table.
# Fill logic after external review:
# - no cross-patient t_hour median/mode fill;
# - use imputed baseline wide-table values for t0 residual gaps;
# - then carry forward within each .imp + stay_id only.

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
  file.path(base_dir, "formal_v1_cov_tvcov_ccw_model_input.csv")
}

wide_imp_file <- if (length(args) >= 3) {
  args[3]
} else {
  file.path(base_dir, "formal_v1_analysis_main_mice_m5_model_ready_long.csv")
}

out_model_ready <- if (length(args) >= 4) {
  args[4]
} else {
  file.path(base_dir, "formal_v1_cov_tvcov_ccw_model_ready_m5.csv")
}
out_summary <- file.path(base_dir, "formal_v1_cov_tvcov_ccw_model_ready_summary.csv")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

library(data.table)

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

to_binary01 <- function(z) {
  zz <- tolower(trimws(as.character(z)))
  out <- rep(NA_integer_, length(zz))
  out[zz %in% c("1", "t", "true", "yes", "y")] <- 1L
  out[zz %in% c("0", "f", "false", "no", "n")] <- 0L
  out
}

mode01 <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(0L)
  tab <- sort(table(x), decreasing = TRUE)
  as.integer(names(tab)[1])
}

require_cols <- function(dat, cols) {
  miss <- setdiff(cols, names(dat))
  if (length(miss) > 0) {
    stop("Missing required column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
}

summarize_values <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(list(
      min = NA_real_, p25 = NA_real_, median = NA_real_,
      p75 = NA_real_, max = NA_real_
    ))
  }
  list(
    min = min(x),
    p25 = as.numeric(quantile(x, 0.25, names = FALSE)),
    median = median(x),
    p75 = as.numeric(quantile(x, 0.75, names = FALSE)),
    max = max(x)
  )
}

make_empty_summary <- function() {
  data.table(
    variable = character(),
    role = character(),
    source_type = character(),
    clean_type = character(),
    lower = numeric(),
    upper = numeric(),
    n = integer(),
    missing_before = integer(),
    parse_invalid_n = integer(),
    extreme_or_invalid_n = integer(),
    missing_after_clean = integer(),
    missing_after_fill = integer(),
    min_before = numeric(),
    max_before = numeric(),
    min_after_clean = numeric(),
    max_after_clean = numeric(),
    fill_rule = character(),
    model_use = character(),
    note = character()
  )
}

apply_numeric_range <- function(dat, var, lower, upper, role, model_use, note) {
  if (!var %in% names(dat)) return(list(dat = dat, summary = make_empty_summary()))

  raw <- dat[[var]]
  miss <- is_miss(raw)
  num <- to_num(raw)
  parse_invalid <- !miss & is.na(num)
  extreme <- !is.na(num) & (num < lower | num > upper)
  flagged <- parse_invalid | extreme
  clean <- num
  clean[flagged] <- NA_real_

  clean_col <- paste0(var, "_clean")
  miss_col <- paste0(var, "_missing_model")
  flag_col <- paste0(var, "_extreme_or_invalid")
  parse_col <- paste0(var, "_parse_invalid")

  dat[, (clean_col) := clean]
  dat[, (miss_col) := as.integer(is.na(clean))]
  dat[, (flag_col) := as.integer(flagged)]
  dat[, (parse_col) := as.integer(parse_invalid)]

  before <- summarize_values(num)
  after <- summarize_values(clean)

  summary <- data.table(
    variable = var,
    role = role,
    source_type = paste(class(raw), collapse = "/"),
    clean_type = "numeric",
    lower = lower,
    upper = upper,
    n = length(raw),
    missing_before = sum(miss),
    parse_invalid_n = sum(parse_invalid),
    extreme_or_invalid_n = sum(flagged),
    missing_after_clean = sum(is.na(clean)),
    missing_after_fill = NA_integer_,
    min_before = before$min,
    max_before = before$max,
    min_after_clean = after$min,
    max_after_clean = after$max,
    fill_rule = "",
    model_use = model_use,
    note = note
  )

  list(dat = dat, summary = summary)
}

apply_binary <- function(dat, var, role, model_use, note) {
  if (!var %in% names(dat)) return(list(dat = dat, summary = make_empty_summary()))

  raw <- dat[[var]]
  miss <- is_miss(raw)
  clean <- to_binary01(raw)
  parse_invalid <- !miss & is.na(clean)

  clean_col <- paste0(var, "_i")
  miss_col <- paste0(var, "_missing_model")
  parse_col <- paste0(var, "_parse_invalid")

  dat[, (clean_col) := clean]
  dat[, (miss_col) := as.integer(is.na(clean))]
  dat[, (parse_col) := as.integer(parse_invalid)]

  summary <- data.table(
    variable = var,
    role = role,
    source_type = paste(class(raw), collapse = "/"),
    clean_type = "binary01",
    lower = 0,
    upper = 1,
    n = length(raw),
    missing_before = sum(miss),
    parse_invalid_n = sum(parse_invalid),
    extreme_or_invalid_n = sum(parse_invalid),
    missing_after_clean = sum(is.na(clean)),
    missing_after_fill = NA_integer_,
    min_before = suppressWarnings(min(to_num(raw), na.rm = TRUE)),
    max_before = suppressWarnings(max(to_num(raw), na.rm = TRUE)),
    min_after_clean = suppressWarnings(min(clean, na.rm = TRUE)),
    max_after_clean = suppressWarnings(max(clean, na.rm = TRUE)),
    fill_rule = "",
    model_use = model_use,
    note = note
  )
  summary[is.infinite(min_before), `:=`(min_before = NA_real_, max_before = NA_real_)]
  summary[is.infinite(min_after_clean), `:=`(min_after_clean = NA_real_, max_after_clean = NA_real_)]

  list(dat = dat, summary = summary)
}

fill_numeric_locf <- function(dat, clean_col, fill_col, baseline_col = NULL) {
  if (!clean_col %in% names(dat)) return(dat)
  setorder(dat, .imp, stay_id, t_hour)
  dat[, (fill_col) := get(clean_col)]
  if (!is.null(baseline_col) && baseline_col %in% names(dat)) {
    dat[
      t_hour == 0L & is.na(get(fill_col)) & !is.na(get(baseline_col)),
      (fill_col) := get(baseline_col)
    ]
  }
  dat[, (fill_col) := nafill(get(fill_col), type = "locf"), by = .(.imp, stay_id)]
  dat
}

fill_binary_locf <- function(dat, clean_col, fill_col, baseline_col = NULL) {
  if (!clean_col %in% names(dat)) return(dat)
  setorder(dat, .imp, stay_id, t_hour)
  dat[, (fill_col) := get(clean_col)]
  if (!is.null(baseline_col) && baseline_col %in% names(dat)) {
    dat[
      t_hour == 0L & is.na(get(fill_col)) & !is.na(get(baseline_col)),
      (fill_col) := get(baseline_col)
    ]
  }
  dat[, (fill_col) := nafill(get(fill_col), type = "locf"), by = .(.imp, stay_id)]
  dat
}

input <- fread(input_file, showProgress = FALSE)
wide_imp <- fread(wide_imp_file, showProgress = FALSE)

required <- c(
  "stay_id", "subject_id", "hadm_id", "sepsis_t0", "t_hour", "grid_time",
  "time_from_t0_h", "still_observed", "sofa_total_tvcov",
  "meanbp_min_tvcov", "shock_tvcov_b", "lactate_tvcov",
  "vent_tvcov", "abx_started_by_hour", "at_risk_for_initiation",
  "first_abx_delay_hours"
)
require_cols(input, required)
require_cols(
  wide_imp,
  c(".imp", "stay_id", "sofa_total_t0_imp", "meanbp_min_t0_imp_sens", "shock_t0_b_factor")
)

setorder(input, stay_id, t_hour)

summary_list <- list()

binary_rules <- list(
  list("still_observed", "risk/process", "main", "Observed at this grid time; no imputation, type only."),
  list("abx_started_by_hour", "exposure process", "rule variable", "Exposure process field; do not use as ordinary confounder."),
  list("at_risk_for_initiation", "exposure process", "rule variable", "Risk-set process field; do not use as ordinary confounder."),
  list("shock_tvcov_b", "time-varying covariate", "main IPCW", "Vasopressor/shock state; type conversion plus missing indicator."),
  list("vent_tvcov", "time-varying covariate", "sensitivity IPCW", "Ventilation status; type conversion plus missing indicator."),
  list("sofa_total_missing_orig", "missingness flag", "diagnostic/sensitivity", "Original SOFA missingness flag from SQL."),
  list("meanbp_missing_orig", "missingness flag", "diagnostic/sensitivity", "Original meanbp missingness flag from SQL."),
  list("lactate_missing_orig", "missingness flag", "diagnostic/sensitivity", "Original lactate missingness flag from SQL."),
  list("sofa_tvcov_missing", "missingness flag", "main IPCW", "Time-varying SOFA missingness flag."),
  list("shock_tvcov_missing", "missingness flag", "main IPCW", "Time-varying shock source missingness flag."),
  list("lactate_tvcov_missing", "missingness flag", "sensitivity IPCW", "Time-varying lactate missingness flag.")
)

for (rule in binary_rules) {
  res <- apply_binary(input, rule[[1]], rule[[2]], rule[[3]], rule[[4]])
  input <- res$dat
  summary_list[[length(summary_list) + 1L]] <- res$summary
}

numeric_rules <- data.table(
  variable = c(
    "t_hour",
    "time_from_t0_h",
    "sofa_total_tvcov",
    "meanbp_min_tvcov",
    "respiration_24hours_tvcov",
    "coagulation_24hours_tvcov",
    "liver_24hours_tvcov",
    "cardiovascular_24hours_tvcov",
    "cns_24hours_tvcov",
    "renal_24hours_tvcov",
    "gcs_min_tvcov",
    "uo_24hr_tvcov",
    "lactate_tvcov",
    "first_abx_delay_hours"
  ),
  lower = c(0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 3, 0, 0.2, 0),
  upper = c(24, 24, 24, 200, 4, 4, 4, 4, 4, 4, 15, 20000, 30, 72),
  role = c(
    "time index",
    "time index",
    "time-varying covariate",
    "time-varying covariate",
    "SOFA component",
    "SOFA component",
    "SOFA component",
    "SOFA component",
    "SOFA component",
    "SOFA component",
    "sparse covariate",
    "sparse covariate",
    "sensitivity covariate",
    "exposure definition"
  ),
  model_use = c(
    "time term",
    "time term",
    "main IPCW",
    "sensitivity IPCW",
    "extension/sensitivity",
    "extension/sensitivity",
    "extension/sensitivity",
    "extension/sensitivity",
    "extension/sensitivity",
    "extension/sensitivity",
    "descriptive/sensitivity only",
    "descriptive/sensitivity only",
    "sensitivity IPCW",
    "rule variable only"
  ),
  note = c(
    "Expected 0-24 hourly grid.",
    "Expected 0-24 hours from t0.",
    "Plausible SOFA total 0-24; low missing after SQL anchoring/LOCF.",
    "Plausible mean arterial pressure 5-200 mmHg; do not force into main outcome model.",
    "SOFA component range 0-4.",
    "SOFA component range 0-4.",
    "SOFA component range 0-4.",
    "SOFA component range 0-4.",
    "SOFA component range 0-4.",
    "SOFA component range 0-4.",
    "High missingness; not for main IPCW.",
    "High missingness; not for main IPCW.",
    "Likely mmol/L; values outside 0.2-30 treated as implausible.",
    "Exposure timing source; no fill for causal definition."
  )
)

for (i in seq_len(nrow(numeric_rules))) {
  rule <- numeric_rules[i]
  res <- apply_numeric_range(
    input,
    rule$variable,
    rule$lower,
    rule$upper,
    rule$role,
    rule$model_use,
    rule$note
  )
  input <- res$dat
  summary_list[[length(summary_list) + 1L]] <- res$summary
}

baseline <- wide_imp[
  .imp %in% 1:5,
  .(
    .imp = as.integer(.imp),
    stay_id,
    baseline_sofa_total_t0_imp = to_num(sofa_total_t0_imp),
    baseline_meanbp_min_t0_imp_sens = to_num(meanbp_min_t0_imp_sens),
    baseline_shock_t0_b_i = as.integer(as.character(shock_t0_b_factor) == "shock")
  )
]

if (baseline[, anyDuplicated(paste(.imp, stay_id))] > 0) {
  stop("Duplicate .imp + stay_id rows in imputed baseline wide table.", call. = FALSE)
}

input <- merge(input, baseline, by = "stay_id", allow.cartesian = TRUE, sort = FALSE)
setorder(input, .imp, stay_id, t_hour)

# Model-facing simple fills. These are technical fills for regression design
# matrices, not a second imputation analysis.
input <- fill_numeric_locf(
  input,
  "sofa_total_tvcov_clean",
  "sofa_total_tvcov_fill",
  "baseline_sofa_total_t0_imp"
)
input <- fill_numeric_locf(
  input,
  "meanbp_min_tvcov_clean",
  "meanbp_min_tvcov_fill",
  "baseline_meanbp_min_t0_imp_sens"
)
input <- fill_numeric_locf(input, "lactate_tvcov_clean", "lactate_tvcov_fill")
input <- fill_binary_locf(
  input,
  "shock_tvcov_b_i",
  "shock_tvcov_b_fill",
  "baseline_shock_t0_b_i"
)
input <- fill_binary_locf(input, "vent_tvcov_i", "vent_tvcov_fill")

input[, t_hour_factor := factor(t_hour_clean, levels = 0:24)]
input[, ipcw_time_factor := factor(t_hour_clean, levels = 0:12)]

summary <- rbindlist(summary_list, fill = TRUE)

fill_pairs <- data.table(
  variable = c(
    "sofa_total_tvcov",
    "meanbp_min_tvcov",
    "lactate_tvcov",
    "shock_tvcov_b",
    "vent_tvcov"
  ),
  fill_col = c(
    "sofa_total_tvcov_fill",
    "meanbp_min_tvcov_fill",
    "lactate_tvcov_fill",
    "shock_tvcov_b_fill",
    "vent_tvcov_fill"
  ),
  fill_rule = c(
    "t0 residual NA filled from imputed baseline wide table, then stay-level LOCF within each .imp",
    "t0 residual NA filled from imputed baseline wide table, then stay-level LOCF within each .imp",
    "stay-level LOCF within each .imp; no cross-patient fill; sensitivity only",
    "t0 residual NA filled from imputed baseline wide table, then stay-level LOCF within each .imp",
    "stay-level LOCF within each .imp; no cross-patient fill; sensitivity only"
  )
)

for (i in seq_len(nrow(fill_pairs))) {
  v <- fill_pairs$variable[i]
  fill_col <- fill_pairs$fill_col[i]
  if (fill_col %in% names(input)) {
    summary[
      variable == v,
      `:=`(
        missing_after_fill = sum(is.na(input[[fill_col]])),
        fill_rule = fill_pairs$fill_rule[i]
      )
    ]
  }
}

main_ipcw_columns <- c(
  ".imp",
  "stay_id",
  "t_hour_clean",
  "t_hour_factor",
  "ipcw_time_factor",
  "still_observed_i",
  "sofa_total_tvcov_fill",
  "sofa_total_tvcov_missing_model",
  "shock_tvcov_b_fill",
  "shock_tvcov_b_missing_model",
  "shock_tvcov_missing_i"
)
missing_main_cols <- setdiff(main_ipcw_columns, names(input))
if (length(missing_main_cols) > 0) {
  stop("Missing expected main IPCW model column(s): ", paste(missing_main_cols, collapse = ", "), call. = FALSE)
}

main_na <- input[
  still_observed_i == 1L & t_hour_clean <= 12,
  lapply(.SD, function(z) sum(is.na(z))),
  .SDcols = main_ipcw_columns
]
if (any(unlist(main_na) > 0)) {
  stop(
    "Main IPCW columns still contain NA after simple fills: ",
    paste(names(main_na)[unlist(main_na) > 0], collapse = ", "),
    call. = FALSE
  )
}

fwrite(input, out_model_ready)
fwrite(summary, out_summary)

cat("formal_v1 long-table model-ready cleaning complete\n")
cat("Input:", input_file, "\n")
cat("Imputed baseline:", wide_imp_file, "\n")
cat("Output:", out_model_ready, "\n")
cat("Summary:", out_summary, "\n")
cat("Rows:", nrow(input), "\n")
cat("Columns:", ncol(input), "\n")
