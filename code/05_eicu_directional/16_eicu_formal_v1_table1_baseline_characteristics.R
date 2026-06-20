# eICU formal_v1 step 16: arm2 Table 1 baseline characteristics
# MIMIC counterpart: 16_formal_v1_table1_baseline_characteristics.R
#
# Scope:
#   Build Table 1 for the eICU wide-table arm2 model set:
#     0-3h vs 3-12h antibiotic start.
#
# Important:
#   This Table 1 uses the pre-imputation cleaned wide table so missingness is
#   visible. Imputed model outputs are used for Step 10 models, not for
#   descriptive baseline distributions.
#
# Input:
#   outputs/wide_tables/eicu_formal_v1_arm2_mice_input.csv
#
# Outputs:
#   outputs/tables/eicu_formal_v1_arm2_table1_baseline.csv
#   outputs/tables/eicu_formal_v1_arm2_table1_missingness.csv
#   outputs/tables/eicu_formal_v1_arm2_table1_smd_rank.csv
#   outputs/tables/eicu_formal_v1_arm2_table1_report.txt

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
  file.path(base_dir, "outputs", "wide_tables", "eicu_formal_v1_arm2_mice_input.csv")
}

out_dir <- file.path(base_dir, "outputs", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_table1 <- file.path(out_dir, "eicu_formal_v1_arm2_table1_baseline.csv")
out_missingness <- file.path(out_dir, "eicu_formal_v1_arm2_table1_missingness.csv")
out_smd_rank <- file.path(out_dir, "eicu_formal_v1_arm2_table1_smd_rank.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_arm2_table1_report.txt")

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
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

fmt_n_pct <- function(n, denom) {
  if (is.na(denom) || denom == 0) return(paste0(n, " (NA)"))
  paste0(n, " (", sprintf("%.1f", 100 * n / denom), "%)")
}

fmt_median_iqr <- function(z) {
  z <- z[!is.na(z)]
  if (length(z) == 0) return("NA")
  q <- stats::quantile(z, probs = c(0.25, 0.50, 0.75), na.rm = TRUE, names = FALSE, type = 2)
  paste0(sprintf("%.1f", q[2]), " [", sprintf("%.1f", q[1]), ", ", sprintf("%.1f", q[3]), "]")
}

continuous_smd <- function(x, arm) {
  x0 <- x[arm == "0-3h"]
  x1 <- x[arm == "3-12h"]
  m0 <- mean(x0, na.rm = TRUE)
  m1 <- mean(x1, na.rm = TRUE)
  s0 <- stats::sd(x0, na.rm = TRUE)
  s1 <- stats::sd(x1, na.rm = TRUE)
  pooled <- sqrt((s0^2 + s1^2) / 2)
  if (!is.finite(pooled) || pooled == 0) return(NA_real_)
  (m1 - m0) / pooled
}

binary_smd <- function(p0, p1) {
  pooled <- sqrt(((p0 * (1 - p0)) + (p1 * (1 - p1))) / 2)
  if (!is.finite(pooled) || pooled == 0) return(NA_real_)
  (p1 - p0) / pooled
}

cat_label <- function(z, missing_level = "Missing") {
  zz <- trimws(as.character(z))
  zz[is_miss(zz)] <- missing_level
  zz
}

bin_label <- function(z, missing_level = "Missing") {
  zi <- as_int01(z)
  out <- rep(missing_level, length(zi))
  out[zi == 0L] <- "No"
  out[zi == 1L] <- "Yes"
  out
}

summarize_continuous <- function(dat, variable, label, section) {
  x <- to_num(dat[[variable]])
  arm <- dat$arm2_abx_timing
  n_all <- length(x)
  n_early <- sum(arm == "0-3h")
  n_late <- sum(arm == "3-12h")
  miss_all <- sum(is.na(x))
  miss_early <- sum(is.na(x[arm == "0-3h"]))
  miss_late <- sum(is.na(x[arm == "3-12h"]))
  p_val <- tryCatch(
    stats::wilcox.test(x ~ arm, exact = FALSE)$p.value,
    error = function(e) NA_real_
  )
  data.frame(
    section = section,
    variable = label,
    level = "",
    type = "continuous",
    overall = paste0(fmt_median_iqr(x), "; missing ", fmt_n_pct(miss_all, n_all)),
    early_0_3h = paste0(fmt_median_iqr(x[arm == "0-3h"]), "; missing ", fmt_n_pct(miss_early, n_early)),
    late_3_12h = paste0(fmt_median_iqr(x[arm == "3-12h"]), "; missing ", fmt_n_pct(miss_late, n_late)),
    nonmissing_overall_n = sum(!is.na(x)),
    missing_overall_n = miss_all,
    missing_overall_pct = round(100 * miss_all / n_all, 2),
    smd = round(continuous_smd(x, arm), 3),
    abs_smd = round(abs(continuous_smd(x, arm)), 3),
    p_value = p_val,
    stringsAsFactors = FALSE
  )
}

summarize_categorical <- function(dat, values, label, section, ordered_levels = NULL) {
  arm <- dat$arm2_abx_timing
  vals <- cat_label(values)
  if (is.null(ordered_levels)) {
    ordered_levels <- names(sort(table(vals), decreasing = TRUE))
  }
  ordered_levels <- c(intersect(ordered_levels, unique(vals)), setdiff(sort(unique(vals)), ordered_levels))
  n_all <- length(vals)
  n_early <- sum(arm == "0-3h")
  n_late <- sum(arm == "3-12h")
  p_val <- tryCatch(
    stats::chisq.test(table(vals, arm))$p.value,
    warning = function(w) suppressWarnings(stats::chisq.test(table(vals, arm))$p.value),
    error = function(e) NA_real_
  )
  rows <- lapply(ordered_levels, function(lv) {
    n_all_lv <- sum(vals == lv)
    n_early_lv <- sum(vals[arm == "0-3h"] == lv)
    n_late_lv <- sum(vals[arm == "3-12h"] == lv)
    p0 <- if (n_early > 0) n_early_lv / n_early else NA_real_
    p1 <- if (n_late > 0) n_late_lv / n_late else NA_real_
    smd <- binary_smd(p0, p1)
    data.frame(
      section = section,
      variable = label,
      level = lv,
      type = "categorical",
      overall = fmt_n_pct(n_all_lv, n_all),
      early_0_3h = fmt_n_pct(n_early_lv, n_early),
      late_3_12h = fmt_n_pct(n_late_lv, n_late),
      nonmissing_overall_n = sum(vals != "Missing"),
      missing_overall_n = sum(vals == "Missing"),
      missing_overall_pct = round(100 * sum(vals == "Missing") / n_all, 2),
      smd = round(smd, 3),
      abs_smd = round(abs(smd), 3),
      p_value = p_val,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

x <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c(
  "patientunitstayid",
  "hospitalid",
  "death_30d_inhosp",
  "arm2_abx_timing",
  "arm2_late_3_12h",
  "age_model",
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "apache_score_iva_model",
  "vent_model",
  "intubated_model",
  "dialysis_model",
  "diabetes_model",
  "cirrhosis_model",
  "immunosuppression_model",
  "hepaticfailure_model",
  "metastaticcancer_model"
)

missing_required <- setdiff(required_cols, names(x))
if (length(missing_required) > 0) {
  stop("Missing required column(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
}

d <- x[cat_label(x$arm2_abx_timing) %in% c("0-3h", "3-12h"), , drop = FALSE]
d$arm2_abx_timing <- cat_label(d$arm2_abx_timing)
d$arm2_late_3_12h <- as_int01(d$arm2_late_3_12h)
d$death_30d_inhosp <- as_int01(d$death_30d_inhosp)

if (any(is.na(d$death_30d_inhosp))) {
  d <- d[!is.na(d$death_30d_inhosp), , drop = FALSE]
}

continuous_vars <- data.frame(
  variable = c("age_model", "apache_score_iva_model"),
  label = c("Age, years", "APACHE IVa score"),
  section = c("Demographics", "Severity"),
  stringsAsFactors = FALSE
)

table_rows <- list()
table_rows[[length(table_rows) + 1L]] <- data.frame(
  section = "Cohort",
  variable = "N",
  level = "",
  type = "count",
  overall = as.character(nrow(d)),
  early_0_3h = as.character(sum(d$arm2_abx_timing == "0-3h")),
  late_3_12h = as.character(sum(d$arm2_abx_timing == "3-12h")),
  nonmissing_overall_n = nrow(d),
  missing_overall_n = 0,
  missing_overall_pct = 0,
  smd = NA_real_,
  abs_smd = NA_real_,
  p_value = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(continuous_vars))) {
  table_rows[[length(table_rows) + 1L]] <- summarize_continuous(
    d,
    continuous_vars$variable[i],
    continuous_vars$label[i],
    continuous_vars$section[i]
  )
}

table_rows[[length(table_rows) + 1L]] <- summarize_categorical(
  d, d$gender_factor, "Sex", "Demographics", ordered_levels = c("Female", "Male", "Missing")
)
table_rows[[length(table_rows) + 1L]] <- summarize_categorical(
  d, d$ethnicity_group, "Ethnicity", "Demographics", ordered_levels = c("Caucasian", "African American", "Hispanic", "Asian", "Native American", "Other/Unknown", "Missing")
)
table_rows[[length(table_rows) + 1L]] <- summarize_categorical(
  d, d$unit_admit_source_group, "Unit admission source", "Admission", ordered_levels = c("Emergency Department", "Floor/Acute Care", "Transfer", "Direct Admit", "Perioperative", "Other", "Missing")
)
table_rows[[length(table_rows) + 1L]] <- summarize_categorical(
  d, d$unit_type_group, "Unit type", "Admission", ordered_levels = c("Med-Surg ICU", "MICU", "SICU", "Cardiac/Surgical Cardiac ICU", "Other ICU", "Missing")
)
table_rows[[length(table_rows) + 1L]] <- summarize_categorical(
  d, d$hosp_window_volume_bucket_factor, "Hospital timing-volume bucket", "Hospital reporting", ordered_levels = c(">30", "11-30", "1-10", "0", "Missing")
)

binary_specs <- data.frame(
  variable = c(
    "vent_model",
    "intubated_model",
    "dialysis_model",
    "diabetes_model",
    "cirrhosis_model",
    "immunosuppression_model",
    "hepaticfailure_model",
    "metastaticcancer_model"
  ),
  label = c(
    "Mechanical ventilation",
    "Intubated",
    "Dialysis",
    "Diabetes",
    "Cirrhosis",
    "Immunosuppression",
    "Hepatic failure",
    "Metastatic cancer"
  ),
  section = c(
    "Severity",
    "Severity",
    "Severity",
    "Comorbidity",
    "Comorbidity",
    "Comorbidity",
    "Comorbidity",
    "Comorbidity"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(binary_specs))) {
  table_rows[[length(table_rows) + 1L]] <- summarize_categorical(
    d,
    bin_label(d[[binary_specs$variable[i]]]),
    binary_specs$label[i],
    binary_specs$section[i],
    ordered_levels = c("No", "Yes", "Missing")
  )
}

table1 <- do.call(rbind, table_rows)
write.csv(table1, out_table1, row.names = FALSE, na = "")

all_missing_vars <- c(
  continuous_vars$variable,
  binary_specs$variable,
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor"
)

missingness <- do.call(rbind, lapply(all_missing_vars, function(v) {
  values <- if (v %in% binary_specs$variable) bin_label(d[[v]]) else cat_label(d[[v]])
  if (v %in% continuous_vars$variable) {
    miss <- is.na(to_num(d[[v]]))
  } else {
    miss <- values == "Missing"
  }
  data.frame(
    variable = v,
    missing_n = sum(miss),
    missing_pct = round(100 * mean(miss), 2),
    early_missing_n = sum(miss[d$arm2_abx_timing == "0-3h"]),
    early_missing_pct = round(100 * mean(miss[d$arm2_abx_timing == "0-3h"]), 2),
    late_missing_n = sum(miss[d$arm2_abx_timing == "3-12h"]),
    late_missing_pct = round(100 * mean(miss[d$arm2_abx_timing == "3-12h"]), 2),
    stringsAsFactors = FALSE
  )
}))
write.csv(missingness, out_missingness, row.names = FALSE, na = "")

smd_rank <- aggregate(abs_smd ~ section + variable, data = table1, FUN = max, na.rm = TRUE)
smd_rank <- smd_rank[order(-smd_rank$abs_smd), , drop = FALSE]
write.csv(smd_rank, out_smd_rank, row.names = FALSE, na = "")

death_tab <- aggregate(death_30d_inhosp ~ arm2_abx_timing, data = d, FUN = function(z) {
  paste0(sum(z == 1), "/", length(z), " (", sprintf("%.2f", 100 * mean(z == 1)), "%)")
})

report_lines <- c(
  "eICU formal_v1 arm2 Table 1 report",
  paste("input:", input_file),
  "",
  "analysis set:",
  paste("N:", nrow(d)),
  paste("distinct stays:", length(unique(d$patientunitstayid))),
  paste("hospitals:", length(unique(d$hospitalid))),
  paste("0-3h N:", sum(d$arm2_abx_timing == "0-3h")),
  paste("3-12h N:", sum(d$arm2_abx_timing == "3-12h")),
  "",
  "outcome check, descriptive only:",
  capture.output(print(death_tab, row.names = FALSE)),
  "",
  "largest baseline imbalance by max absolute SMD:",
  capture.output(print(head(smd_rank, 20), row.names = FALSE)),
  "",
  "highest missingness:",
  capture.output(print(head(missingness[order(-missingness$missing_pct), ], 20), row.names = FALSE)),
  "",
  "outputs:",
  out_table1,
  out_missingness,
  out_smd_rank
)
writeLines(report_lines, out_report)

message("Done. Table 1: ", out_table1)
message("Report: ", out_report)
