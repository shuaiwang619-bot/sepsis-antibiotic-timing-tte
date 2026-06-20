# eICU formal_v1 step 05: wide-table MICE imputation
# MIMIC counterpart: 05_formal_v1_analysis_main_mice_imputation.R
#
# Scope:
#   Read the Step 04 cleaned wide table, restrict to the arm2 analysis set,
#   and impute eligible baseline covariates only.
#
# Important:
#   This script does not impute exposure, outcome, IDs, or hospitalid.
#   Exposure and outcome are included as predictors.
#
# Default:
#   m = 5, maxit = 20, seed = 2025.
#
# Input:
#   outputs/wide_tables/eicu_formal_v1_analysis_main_extreme_cleaned.csv
#
# Outputs:
#   outputs/mice/eicu_formal_v1_arm2_model_dataset_mice_m5_long.csv
#   outputs/mice/eicu_formal_v1_arm2_model_dataset_mice_m5_model_ready.csv
#   outputs/mice/eicu_formal_v1_mice_missingness_report.csv
#   outputs/mice/eicu_formal_v1_mice_method_matrix.csv

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
  file.path(base_dir, "outputs", "wide_tables", "eicu_formal_v1_analysis_main_extreme_cleaned.csv")
}
m_imp <- if (length(args) >= 3) as.integer(args[3]) else 5L
maxit <- if (length(args) >= 4) as.integer(args[4]) else 20L
seed <- if (length(args) >= 5) as.integer(args[5]) else 2025L

out_dir <- file.path(base_dir, "outputs", "mice")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_long <- file.path(out_dir, "eicu_formal_v1_arm2_model_dataset_mice_m5_long.csv")
out_model_ready <- file.path(out_dir, "eicu_formal_v1_arm2_model_dataset_mice_m5_model_ready.csv")
out_missingness <- file.path(out_dir, "eicu_formal_v1_mice_missingness_report.csv")
out_method_matrix <- file.path(out_dir, "eicu_formal_v1_mice_method_matrix.csv")
out_predictor_matrix <- file.path(out_dir, "eicu_formal_v1_mice_predictor_matrix.csv")
out_binary_audit <- file.path(out_dir, "eicu_formal_v1_mice_binary_candidate_audit.csv")
out_variable_plan <- file.path(out_dir, "eicu_formal_v1_mice_variable_plan.csv")
out_imputed_values <- file.path(out_dir, "eicu_formal_v1_arm2_model_dataset_mice_m5_imputed_values.csv")
out_summary <- file.path(out_dir, "eicu_formal_v1_arm2_model_dataset_mice_m5_summary.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_arm2_model_dataset_mice_m5_report.txt")
out_mids <- file.path(out_dir, "eicu_formal_v1_arm2_model_dataset_mice_m5_mids.rds")

if (!requireNamespace("mice", quietly = TRUE)) {
  stop("Package 'mice' is required. Install it before running this script.", call. = FALSE)
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

gt0_or0 <- function(z) {
  znum <- to_num(z)
  as.integer(!is.na(znum) & znum > 0)
}

as_binary_factor <- function(z) {
  factor(as_int01(z), levels = c(0L, 1L))
}

as_factor_missing <- function(z, missing_level = "Missing") {
  zz <- trimws(as.character(z))
  zz[is_miss(zz)] <- missing_level
  droplevels(factor(zz))
}

has_variation <- function(v) {
  vv <- as.character(v)
  vv <- vv[!is.na(vv)]
  length(unique(vv)) > 1L
}

completed_vector <- function(v) {
  if (is.factor(v)) {
    vv <- as.character(v)
    if (all(is.na(vv) | vv %in% c("0", "1"))) {
      return(as.integer(vv))
    }
    return(vv)
  }
  v
}

if (!file.exists(input_file)) {
  fallback <- file.path(base_dir, "outputs", "wide_tables", "eicu_formal_v1_arm2_mice_input.csv")
  if (file.exists(fallback)) {
    input_file <- fallback
  } else {
    stop("Input file not found: ", input_file, call. = FALSE)
  }
}

x0 <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
if ("arm2_model_ind" %in% names(x0)) {
  x <- x0[which(to_num(x0$arm2_model_ind) == 1), , drop = FALSE]
} else {
  x <- x0
}

required_cols <- c(
  "patientunitstayid",
  "uniquepid",
  "hospitalid",
  "death_30d_inhosp",
  "arm2_abx_timing",
  "arm2_late_3_12h",
  "first_abx_offset_min",
  "first_abx_delay_hours_from_unit",
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "hosp_zero_abx_reporting_ind",
  "age_model",
  "age_model_missing_ind",
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

y <- data.frame(
  patientunitstayid = x$patientunitstayid,
  uniquepid = x$uniquepid,
  hospitalid = x$hospitalid,

  death_30d_inhosp = as.integer(to_num(x$death_30d_inhosp)),
  arm2_abx_timing = as_factor_missing(x$arm2_abx_timing),
  arm2_late_3_12h = as.integer(to_num(x$arm2_late_3_12h)),

  first_abx_offset_min = to_num(x$first_abx_offset_min),
  first_abx_delay_hours_from_unit = to_num(x$first_abx_delay_hours_from_unit),

  gender_factor = as_factor_missing(x$gender_factor),
  ethnicity_group = as_factor_missing(x$ethnicity_group),
  unit_admit_source_group = as_factor_missing(x$unit_admit_source_group),
  unit_type_group = as_factor_missing(x$unit_type_group),
  hosp_window_volume_bucket_factor = as_factor_missing(x$hosp_window_volume_bucket_factor),
  hosp_zero_abx_reporting_ind = gt0_or0(x$hosp_zero_abx_reporting_ind),

  age_model = to_num(x$age_model),
  age_model_missing_ind = gt0_or0(x$age_model_missing_ind),

  apache_score_iva_model = to_num(x$apache_score_iva_model),
  apache_score_iva_model_missing_ind = gt0_or0(x$apache_score_iva_model_missing_ind),

  vent_model = as_binary_factor(x$vent_model),
  vent_model_missing_ind = gt0_or0(x$vent_model_missing_ind),

  intubated_model = as_binary_factor(x$intubated_model),
  intubated_model_missing_ind = gt0_or0(x$intubated_model_missing_ind),

  dialysis_model = as_binary_factor(x$dialysis_model),
  dialysis_model_missing_ind = gt0_or0(x$dialysis_model_missing_ind),

  diabetes_model = as_binary_factor(x$diabetes_model),
  diabetes_model_missing_ind = gt0_or0(x$diabetes_model_missing_ind),

  cirrhosis_model = as_binary_factor(x$cirrhosis_model),
  cirrhosis_model_missing_ind = gt0_or0(x$cirrhosis_model_missing_ind),

  immunosuppression_model = as_binary_factor(x$immunosuppression_model),
  immunosuppression_model_missing_ind = gt0_or0(x$immunosuppression_model_missing_ind),

  hepaticfailure_model = as_binary_factor(x$hepaticfailure_model),
  hepaticfailure_model_missing_ind = gt0_or0(x$hepaticfailure_model_missing_ind),

  metastaticcancer_model = as_binary_factor(x$metastaticcancer_model),
  metastaticcancer_model_missing_ind = gt0_or0(x$metastaticcancer_model_missing_ind),

  stringsAsFactors = FALSE
)

optional_sensitivity_cols <- c(
  "heartrate_apache_model",
  "meanbp_apache_model",
  "respiratoryrate_apache_model",
  "temperature_apache_model",
  "creatinine_apache_model",
  "wbc_apache_model",
  "sodium_apache_model",
  "glucose_apache_model"
)
for (nm in optional_sensitivity_cols) {
  miss_nm <- paste0(nm, "_missing_ind")
  if (nm %in% names(x) && miss_nm %in% names(x)) {
    y[[nm]] <- to_num(x[[nm]])
    y[[miss_nm]] <- gt0_or0(x[[miss_nm]])
  }
}

if (any(is.na(y$death_30d_inhosp))) {
  stop("Outcome has missing values after arm2 restriction. Check Step 04 arm2_model_ind.", call. = FALSE)
}
if (any(is.na(y$arm2_late_3_12h))) {
  stop("Exposure has missing values after arm2 restriction. Check Step 04 arm2 labels.", call. = FALSE)
}

continuous_targets <- c("age_model", "apache_score_iva_model")

sensitivity_continuous_vars <- c(
  "heartrate_apache_model",
  "meanbp_apache_model",
  "respiratoryrate_apache_model",
  "temperature_apache_model",
  "creatinine_apache_model",
  "wbc_apache_model",
  "sodium_apache_model",
  "glucose_apache_model"
)
sensitivity_continuous_vars <- intersect(sensitivity_continuous_vars, names(y))

binary_targets_planned <- c(
  "vent_model",
  "intubated_model",
  "dialysis_model",
  "diabetes_model"
)

indicator_only_binary_vars <- c(
  "cirrhosis_model",
  "immunosuppression_model",
  "hepaticfailure_model",
  "metastaticcancer_model"
)

all_model_vars <- c(
  continuous_targets,
  sensitivity_continuous_vars,
  binary_targets_planned,
  indicator_only_binary_vars
)

target_vars_planned <- c(continuous_targets, binary_targets_planned)
missing_indicator_for_target <- setNames(
  paste0(all_model_vars, "_missing_ind"),
  all_model_vars
)

mice_dat <- y[, c(
  continuous_targets,
  binary_targets_planned,
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "hosp_zero_abx_reporting_ind",
  "arm2_late_3_12h",
  "death_30d_inhosp"
)]

stopifnot(nrow(mice_dat) == nrow(y))
stopifnot(!any(grepl("_missing_ind$", names(mice_dat))))
row_key <- y$patientunitstayid

missing_before <- vapply(mice_dat, function(v) sum(is.na(v)), integer(1))
min_binary_positive_n <- 50L
min_binary_missing_n <- 20L

meth <- mice::make.method(mice_dat)
meth[] <- ""

for (v in continuous_targets) {
  if (missing_before[[v]] > 0) {
    meth[[v]] <- "pmm"
  }
}

binary_audit <- do.call(rbind, lapply(c(binary_targets_planned, indicator_only_binary_vars), function(v) {
  values <- as_int01(y[[v]])
  nonmissing_n <- sum(!is.na(values))
  data.frame(
    variable = v,
    planned_role = ifelse(v %in% binary_targets_planned, "candidate_logreg_target", "indicator_only_by_design"),
    n = length(values),
    missing_n = sum(is.na(values)),
    positive_n = sum(values == 1L, na.rm = TRUE),
    positive_pct = if (nonmissing_n > 0) round(100 * sum(values == 1L, na.rm = TRUE) / nonmissing_n, 2) else NA_real_,
    stringsAsFactors = FALSE
  )
}))

for (v in binary_targets_planned) {
  if (missing_before[[v]] > 0) {
    observed_levels <- unique(as.character(mice_dat[[v]][!is.na(mice_dat[[v]])]))
    positive_n <- binary_audit$positive_n[binary_audit$variable == v]
    missing_n <- binary_audit$missing_n[binary_audit$variable == v]
    if (length(observed_levels) == 2L &&
        positive_n >= min_binary_positive_n &&
        missing_n >= min_binary_missing_n) {
      meth[[v]] <- "logreg"
    } else {
      warning("Binary target downgraded to indicator-only: ", v)
    }
  }
}

target_vars <- names(meth)[meth != ""]
downgraded_binary_vars <- setdiff(binary_targets_planned, target_vars)
indicator_only_binary_vars <- unique(c(indicator_only_binary_vars, downgraded_binary_vars))

binary_audit$method <- ifelse(
  binary_audit$variable %in% target_vars,
  meth[binary_audit$variable],
  "indicator_only"
)
binary_audit$threshold_positive_n <- min_binary_positive_n
binary_audit$threshold_missing_n <- min_binary_missing_n
write.csv(binary_audit, out_binary_audit, row.names = FALSE, fileEncoding = "UTF-8")

pred <- mice::make.predictorMatrix(mice_dat)
pred[,] <- 0

candidate_predictors <- names(mice_dat)[vapply(mice_dat, has_variation, logical(1))]
for (target in target_vars) {
  pred[target, setdiff(candidate_predictors, target)] <- 1
  if (target == "apache_score_iva_model" && "gender_factor" %in% colnames(pred)) {
    pred[target, "gender_factor"] <- 0
  }
  this_missing_ind <- missing_indicator_for_target[[target]]
  if (!is.null(this_missing_ind) && this_missing_ind %in% colnames(pred)) {
    pred[target, this_missing_ind] <- 0
  }
}

# hospitalid is intentionally not included in MICE because it is high-cardinality.
# The hospital reporting-volume bucket is included as a low-dimensional proxy.

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
  "patientunitstayid",
  "uniquepid",
  "hospitalid",
  "death_30d_inhosp",
  "arm2_abx_timing",
  "arm2_late_3_12h",
  "first_abx_offset_min",
  "first_abx_delay_hours_from_unit",
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "hosp_zero_abx_reporting_ind",
  unname(missing_indicator_for_target)
)]

for (v in all_model_vars) {
  base_out[[paste0(v, "_raw_for_impute")]] <- completed_vector(y[[v]])
  base_out[[paste0(v, "_was_missing")]] <- as.integer(is.na(y[[v]]))
}

make_completed_out <- function(i) {
  comp <- if (i == 0L) mice_dat else mice::complete(imp, action = i)
  out <- base_out
  out$.imp <- i
  for (v in all_model_vars) {
    value <- if (v %in% names(comp)) completed_vector(comp[[v]]) else completed_vector(y[[v]])
    if (i > 0L && v %in% indicator_only_binary_vars) {
      value <- as_int01(value)
      value[is.na(value)] <- 0L
    }
    out[[v]] <- value
  }
  out <- out[, c(".imp", setdiff(names(out), ".imp"))]
  out
}

completed_list <- lapply(0:m_imp, make_completed_out)
completed_long <- do.call(rbind, completed_list)
write.csv(completed_long, out_long, row.names = FALSE, fileEncoding = "UTF-8")

model_ready_list <- lapply(seq_len(m_imp), make_completed_out)
model_ready <- do.call(rbind, model_ready_list)
write.csv(model_ready, out_model_ready, row.names = FALSE, fileEncoding = "UTF-8")

imputed_rows <- list()
for (i in seq_len(m_imp)) {
  comp <- mice::complete(imp, action = i)
  for (v in target_vars) {
    idx <- which(is.na(mice_dat[[v]]))
    if (length(idx) == 0) next
    imputed_rows[[length(imputed_rows) + 1L]] <- data.frame(
      .imp = i,
      patientunitstayid = row_key[idx],
      variable = v,
      imputed_value = completed_vector(comp[[v]])[idx],
      stringsAsFactors = FALSE
    )
  }
}
imputed_values <- if (length(imputed_rows) > 0) do.call(rbind, imputed_rows) else data.frame()
write.csv(imputed_values, out_imputed_values, row.names = FALSE, fileEncoding = "UTF-8")

role_of_variable <- function(v) {
  if (v %in% target_vars) return("actual_imputation_target")
  if (v %in% setdiff(target_vars_planned, target_vars)) {
    if (v %in% names(y) && sum(is.na(y[[v]])) == 0) return("complete_baseline_no_imputation_needed")
    return("candidate_downgraded_indicator_only")
  }
  if (v %in% sensitivity_continuous_vars) return("sensitivity_only_not_imputed")
  if (v %in% indicator_only_binary_vars) return("indicator_only_not_imputed")
  if (v %in% unname(missing_indicator_for_target)) return("missing_indicator_model_only_not_mice_predictor")
  if (v %in% c("arm2_late_3_12h", "death_30d_inhosp")) return("exposure_or_outcome_predictor_only")
  if (v %in% c(
    "gender_factor", "ethnicity_group", "unit_admit_source_group",
    "unit_type_group", "hosp_window_volume_bucket_factor",
    "hosp_zero_abx_reporting_ind"
  )) {
    return("baseline_predictor_only")
  }
  "other"
}

missingness_report <- data.frame(
  variable = names(mice_dat),
  class = vapply(mice_dat, function(v) paste(class(v), collapse = "|"), character(1)),
  role = vapply(names(mice_dat), role_of_variable, character(1)),
  missing_n = as.integer(missing_before),
  missing_pct = round(100 * as.numeric(missing_before) / nrow(mice_dat), 2),
  method = unname(meth),
  imputed = names(mice_dat) %in% target_vars,
  stringsAsFactors = FALSE
)
write.csv(missingness_report, out_missingness, row.names = FALSE, fileEncoding = "UTF-8")

variable_plan <- do.call(rbind, lapply(all_model_vars, function(v) {
  values <- if (v %in% names(y)) y[[v]] else rep(NA, nrow(y))
  values_num <- suppressWarnings(as.numeric(as.character(values)))
  is_binary <- all(na.omit(unique(values_num)) %in% c(0, 1))
  data.frame(
    variable = v,
    role = role_of_variable(v),
    in_mice_data = v %in% names(mice_dat),
    method = if (v %in% names(meth)) unname(meth[[v]]) else "",
    missing_indicator = unname(missing_indicator_for_target[[v]]),
    missing_n = sum(is.na(values_num)),
    positive_n = if (is_binary) sum(values_num == 1, na.rm = TRUE) else NA_integer_,
    output_handling = ifelse(
      v %in% target_vars,
      "imputed",
      ifelse(v %in% indicator_only_binary_vars, "missing set to 0 plus missing indicator", "observed value retained; missing retained")
    ),
    stringsAsFactors = FALSE
  )
}))
write.csv(variable_plan, out_variable_plan, row.names = FALSE, fileEncoding = "UTF-8")

method_matrix <- data.frame(
  variable = names(meth),
  method = unname(meth),
  missing_n = as.integer(missing_before[names(meth)]),
  missing_pct = round(100 * as.numeric(missing_before[names(meth)]) / nrow(mice_dat), 2),
  imputed = names(meth) %in% target_vars,
  predictor_count = rowSums(pred != 0),
  predictors = vapply(
    rownames(pred),
    function(rn) paste(colnames(pred)[pred[rn, ] != 0], collapse = ";"),
    character(1)
  ),
  stringsAsFactors = FALSE
)
write.csv(method_matrix, out_method_matrix, row.names = FALSE, fileEncoding = "UTF-8")

predictor_matrix_out <- data.frame(variable = rownames(pred), pred, check.names = FALSE)
write.csv(predictor_matrix_out, out_predictor_matrix, row.names = FALSE, fileEncoding = "UTF-8")

summary_rows <- list()
for (v in target_vars_planned) {
  original <- completed_vector(mice_dat[[v]])
  completed_values <- unlist(lapply(seq_len(m_imp), function(i) {
    completed_vector(mice::complete(imp, action = i)[[v]])
  }))
  original_num <- suppressWarnings(as.numeric(original))
  completed_num <- suppressWarnings(as.numeric(completed_values))

  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    variable = v,
    method = meth[[v]],
    m = m_imp,
    maxit = maxit,
    seed = seed,
    missing_before = sum(is.na(original)),
    missing_after_each_completed_dataset = sum(is.na(completed_vector(mice::complete(imp, action = 1)[[v]]))),
    original_min = if (all(is.na(original_num))) NA_real_ else min(original_num, na.rm = TRUE),
    original_median = if (all(is.na(original_num))) NA_real_ else median(original_num, na.rm = TRUE),
    original_max = if (all(is.na(original_num))) NA_real_ else max(original_num, na.rm = TRUE),
    completed_min = if (all(is.na(completed_num))) NA_real_ else min(completed_num, na.rm = TRUE),
    completed_median = if (all(is.na(completed_num))) NA_real_ else median(completed_num, na.rm = TRUE),
    completed_max = if (all(is.na(completed_num))) NA_real_ else max(completed_num, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}
summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, out_summary, row.names = FALSE, fileEncoding = "UTF-8")

saveRDS(imp, out_mids)

sink(out_report)
cat("eICU formal_v1 arm2 wide MICE imputation report\n")
cat("input:", input_file, "\n")
cat("rows in input:", nrow(x0), "\n")
cat("rows in arm2 model set:", nrow(y), "\n")
cat("m:", m_imp, "\n")
cat("maxit:", maxit, "\n")
cat("seed:", seed, "\n\n")

cat("analysis set:\n")
cat("N:", nrow(y), "\n")
cat("Deaths:", sum(y$death_30d_inhosp), "\n")
cat("Hospitals:", length(unique(y$hospitalid)), "\n")
print(table(y$arm2_abx_timing), quote = FALSE)

cat("\nplanned target variables:\n")
print(data.frame(
  variable = target_vars_planned,
  method = meth[target_vars_planned],
  missing_before = missing_before[target_vars_planned],
  row.names = NULL
), row.names = FALSE)

cat("\nvariables actually imputed:\n")
print(data.frame(
  variable = target_vars,
  method = meth[target_vars],
  missing_before = missing_before[target_vars],
  row.names = NULL
), row.names = FALSE)

cat("\nvariables not imputed by design:\n")
cat("IDs: patientunitstayid, uniquepid, hospitalid\n")
cat("Exposure: arm2_abx_timing, arm2_late_3_12h\n")
cat("Outcome: death_30d_inhosp\n")
cat("Hospitalid is not used in MICE; hospital reporting bucket is used instead.\n")
cat("Missing indicators are retained for Step 10 modeling but are excluded from the MICE predictor matrix.\n")
cat("completed_long includes .imp = 0 as the original incomplete data; model_ready includes .imp = 1..m only.\n")

cat("\npredictor matrix nonzero rows:\n")
for (target in target_vars) {
  cat(target, ":", paste(colnames(pred)[pred[target, ] != 0], collapse = ", "), "\n")
}

cat("\nvariable plan:\n")
print(variable_plan, row.names = FALSE)

cat("\nbinary candidate audit:\n")
print(binary_audit, row.names = FALSE)

cat("\nmissingness report:\n")
print(missingness_report, row.names = FALSE)

cat("\nsummary:\n")
print(summary_df, row.names = FALSE)

cat("\noutputs:\n")
cat(out_long, "\n")
cat(out_model_ready, "\n")
cat(out_missingness, "\n")
cat(out_method_matrix, "\n")
cat(out_predictor_matrix, "\n")
cat(out_binary_audit, "\n")
cat(out_variable_plan, "\n")
cat(out_imputed_values, "\n")
cat(out_summary, "\n")
cat(out_mids, "\n")
sink()

cat("Wrote:\n")
cat(out_long, "\n")
cat(out_model_ready, "\n")
cat(out_missingness, "\n")
cat(out_method_matrix, "\n")
cat(out_predictor_matrix, "\n")
cat(out_binary_audit, "\n")
cat(out_variable_plan, "\n")
cat(out_imputed_values, "\n")
cat(out_summary, "\n")
cat(out_report, "\n")
cat(out_mids, "\n")
