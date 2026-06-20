# formal_v1 original analyzable cohort baseline table
# Scope: patient-level analyzable cohort before cloning, censoring, or weighting.
# Groups: observed antibiotic initiation window: 0-3h, 3-6h, 6-12h, >=12h.

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

input_file <- file.path(base_dir, "formal_v1_analysis_main.csv")
out_table <- file.path(base_dir, "formal_v1_table1_original_analyzable_baseline.csv")
out_long <- file.path(base_dir, "formal_v1_table1_original_analyzable_baseline_long.csv")
out_report <- file.path(base_dir, "formal_v1_table1_original_analyzable_baseline_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

to_clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | toupper(x) %in% c("NA", "N/A", "NULL", "NONE")] <- NA_character_
  x
}

fmt_num <- function(x, digits = 1) {
  vapply(x, function(xx) {
    if (!is.finite(xx)) return("")
    formatC(xx, format = "f", digits = digits, big.mark = ",")
  }, character(1))
}

fmt_int <- function(x) {
  vapply(x, function(xx) {
    if (!is.finite(xx)) return("")
    formatC(round(xx), format = "d", big.mark = ",")
  }, character(1))
}

fmt_pct <- function(x, digits = 1) {
  vapply(x, function(xx) {
    if (!is.finite(xx)) return("")
    paste0(formatC(100 * xx, format = "f", digits = digits), "%")
  }, character(1))
}

fmt_smd <- function(x) {
  vapply(x, function(xx) {
    if (!is.finite(xx)) return("")
    formatC(xx, format = "f", digits = 3)
  }, character(1))
}

plain_quantile <- function(x, probs) {
  x <- as_num(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0L) return(rep(NA_real_, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, names = FALSE, na.rm = TRUE, type = 2))
}

format_cont <- function(q25, q50, q75, digits) {
  paste0(fmt_num(q50, digits), " [", fmt_num(q25, digits), ", ", fmt_num(q75, digits), "]")
}

format_cat <- function(n, pct) {
  paste0(fmt_int(n), " (", fmt_pct(pct, 1), ")")
}

max_pairwise_smd_cont <- function(dt, variable, group_var = "abx_window_factor") {
  lev <- levels(dt[[group_var]])
  vals <- vapply(utils::combn(lev, 2, simplify = FALSE), function(pair) {
    x1 <- as_num(dt[get(group_var) == pair[1], get(variable)])
    x2 <- as_num(dt[get(group_var) == pair[2], get(variable)])
    x1 <- x1[is.finite(x1)]
    x2 <- x2[is.finite(x2)]
    if (length(x1) < 2L || length(x2) < 2L) return(NA_real_)
    pooled_sd <- sqrt((stats::var(x1) + stats::var(x2)) / 2)
    if (!is.finite(pooled_sd) || pooled_sd == 0) return(0)
    abs((mean(x1) - mean(x2)) / pooled_sd)
  }, numeric(1))
  max(vals, na.rm = TRUE)
}

max_pairwise_smd_binary <- function(dt, variable, level = "1", group_var = "abx_window_factor") {
  lev <- levels(dt[[group_var]])
  vals <- vapply(utils::combn(lev, 2, simplify = FALSE), function(pair) {
    z1 <- as.character(dt[get(group_var) == pair[1], get(variable)]) == level
    z2 <- as.character(dt[get(group_var) == pair[2], get(variable)]) == level
    z1 <- z1[!is.na(z1)]
    z2 <- z2[!is.na(z2)]
    if (length(z1) == 0L || length(z2) == 0L) return(NA_real_)
    p1 <- mean(z1)
    p2 <- mean(z2)
    pooled <- sqrt((p1 * (1 - p1) + p2 * (1 - p2)) / 2)
    if (!is.finite(pooled) || pooled == 0) return(0)
    abs((p1 - p2) / pooled)
  }, numeric(1))
  max(vals, na.rm = TRUE)
}

stop_if_missing(input_file, "formal_v1 analysis_main")

need_cols <- c(
  "stay_id", "analyzable", "ccw_role", "abx_window",
  "age_at_icu", "gender", "race", "admission_type",
  "charlson_comorbidity_index", "sofa_total_t0",
  "meanbp_min_t0", "shock_t0_b"
)
header <- names(fread(input_file, nrows = 0, showProgress = FALSE))
missing_cols <- setdiff(need_cols, header)
if (length(missing_cols) > 0) {
  stop("analysis_main missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

x <- fread(input_file, select = need_cols, showProgress = FALSE)
if (x[, anyDuplicated(stay_id)] > 0) {
  stop("Duplicate stay_id rows in analysis_main.", call. = FALSE)
}

x[, `:=`(
  analyzable = as.integer(as_num(analyzable)),
  age_at_icu = as_num(age_at_icu),
  charlson_comorbidity_index = as_num(charlson_comorbidity_index),
  sofa_total_t0 = as_num(sofa_total_t0),
  meanbp_min_t0 = as_num(meanbp_min_t0)
)]

race_text <- toupper(to_clean_text(x$race))
race_text_safe <- race_text
race_text_safe[is.na(race_text_safe)] <- ""
x[, race_group := fifelse(is.na(race_text), "UNKNOWN",
  fifelse(grepl("WHITE", race_text_safe), "WHITE",
  fifelse(grepl("BLACK|AFRICAN", race_text_safe), "BLACK",
  fifelse(grepl("ASIAN", race_text_safe), "ASIAN",
  fifelse(grepl("HISPANIC|LATINO|SOUTH AMERICAN", race_text_safe), "HISPANIC",
  fifelse(grepl("UNKNOWN|UNABLE|DECLINED", race_text_safe), "UNKNOWN", "OTHER"))))))]

adm_text <- toupper(to_clean_text(x$admission_type))
x[, admission_type_group := fifelse(is.na(adm_text), "MISSING",
  fifelse(grepl("EMER", adm_text), "EMERGENCY",
  fifelse(grepl("URGENT", adm_text), "URGENT",
  fifelse(grepl("ELECTIVE|SURGICAL", adm_text), "ELECTIVE_SURGICAL",
  fifelse(grepl("OBSERVATION", adm_text), "OBSERVATION", "OTHER")))))]

gender_text <- toupper(to_clean_text(x$gender))
x[, sex_factor := fifelse(gender_text %in% c("M", "MALE"), "M",
  fifelse(gender_text %in% c("F", "FEMALE"), "F", NA_character_))]

shock_text <- toupper(to_clean_text(x$shock_t0_b))
x[, shock_t0_b_num := fifelse(shock_text %in% c("1", "TRUE", "T", "YES", "Y", "SHOCK"), 1L,
  fifelse(shock_text %in% c("0", "FALSE", "F", "NO", "N", "NO_SHOCK"), 0L, as.integer(as_num(shock_t0_b))))]

x[, sofa_total_t0_clean := fifelse(!is.na(sofa_total_t0) & sofa_total_t0 >= 0 & sofa_total_t0 <= 24, sofa_total_t0, NA_real_)]
x[, sofa_total_t0_missing_or_invalid := as.integer(is.na(sofa_total_t0_clean))]

x[, meanbp_min_t0_clean := fifelse(!is.na(meanbp_min_t0) & meanbp_min_t0 >= 5 & meanbp_min_t0 <= 200, meanbp_min_t0, NA_real_)]
x[, meanbp_min_t0_missing := as.integer(is.na(meanbp_min_t0))]
x[, meanbp_min_t0_extreme_or_invalid := as.integer(!is.na(meanbp_min_t0) & is.na(meanbp_min_t0_clean))]

cohort <- x[
  analyzable == 1L &
    ccw_role == "analyzable" &
    abx_window %in% c("0-3h", "3-6h", "6-12h", ">=12h")
]
cohort[, abx_window_factor := factor(abx_window, levels = c("0-3h", "3-6h", "6-12h", ">=12h"))]
cohort[, race_group := factor(race_group, levels = c("WHITE", "BLACK", "ASIAN", "HISPANIC", "OTHER", "UNKNOWN"))]
cohort[, admission_type_group := factor(admission_type_group, levels = c("EMERGENCY", "URGENT", "ELECTIVE_SURGICAL", "OBSERVATION", "OTHER", "MISSING"))]
cohort[, sex_factor := factor(sex_factor, levels = c("F", "M"))]

expected_counts <- c("0-3h" = 3439L, "3-6h" = 3241L, "6-12h" = 5017L, ">=12h" = 6470L)
observed_counts <- cohort[, .N, by = abx_window_factor][order(abx_window_factor)]
if (!identical(setNames(observed_counts$N, as.character(observed_counts$abx_window_factor)), expected_counts)) {
  print(observed_counts)
  stop("Observed analyzable window counts do not match locked formal_v1 counts.", call. = FALSE)
}
if (cohort[, .N] != 18167L) {
  stop("Analyzable cohort size is not 18,167.", call. = FALSE)
}

group_levels <- levels(cohort$abx_window_factor)

section_row <- function(row_id, label) {
  data.table(
    row_id = row_id,
    row_type = "section",
    section = label,
    characteristic = label,
    overall = "",
    window_0_3h = "",
    window_3_6h = "",
    window_6_12h = "",
    window_ge12h = "",
    max_smd = ""
  )
}

cont_row <- function(row_id, label, variable, digits = 1, section = "") {
  q_all <- plain_quantile(cohort[[variable]], c(0.25, 0.50, 0.75))
  by_group <- cohort[, {
    q <- plain_quantile(get(variable), c(0.25, 0.50, 0.75))
    .(q25 = q[1], q50 = q[2], q75 = q[3])
  }, by = abx_window_factor]
  setorder(by_group, abx_window_factor)
  data.table(
    row_id = row_id,
    row_type = "data",
    section = section,
    characteristic = label,
    overall = format_cont(q_all[1], q_all[2], q_all[3], digits),
    window_0_3h = format_cont(by_group[abx_window_factor == "0-3h", q25], by_group[abx_window_factor == "0-3h", q50], by_group[abx_window_factor == "0-3h", q75], digits),
    window_3_6h = format_cont(by_group[abx_window_factor == "3-6h", q25], by_group[abx_window_factor == "3-6h", q50], by_group[abx_window_factor == "3-6h", q75], digits),
    window_6_12h = format_cont(by_group[abx_window_factor == "6-12h", q25], by_group[abx_window_factor == "6-12h", q50], by_group[abx_window_factor == "6-12h", q75], digits),
    window_ge12h = format_cont(by_group[abx_window_factor == ">=12h", q25], by_group[abx_window_factor == ">=12h", q50], by_group[abx_window_factor == ">=12h", q75], digits),
    max_smd = fmt_smd(max_pairwise_smd_cont(cohort, variable))
  )
}

cat_row <- function(row_id, label, variable, level, section = "") {
  is_level_all <- as.character(cohort[[variable]]) == level
  by_group <- cohort[, {
    z <- as.character(get(variable)) == level
    .(n = sum(z, na.rm = TRUE), pct = sum(z, na.rm = TRUE) / .N)
  }, by = abx_window_factor]
  setorder(by_group, abx_window_factor)
  data.table(
    row_id = row_id,
    row_type = "data",
    section = section,
    characteristic = label,
    overall = format_cat(sum(is_level_all, na.rm = TRUE), sum(is_level_all, na.rm = TRUE) / nrow(cohort)),
    window_0_3h = format_cat(by_group[abx_window_factor == "0-3h", n], by_group[abx_window_factor == "0-3h", pct]),
    window_3_6h = format_cat(by_group[abx_window_factor == "3-6h", n], by_group[abx_window_factor == "3-6h", pct]),
    window_6_12h = format_cat(by_group[abx_window_factor == "6-12h", n], by_group[abx_window_factor == "6-12h", pct]),
    window_ge12h = format_cat(by_group[abx_window_factor == ">=12h", n], by_group[abx_window_factor == ">=12h", pct]),
    max_smd = fmt_smd(max_pairwise_smd_binary(cohort, variable, level))
  )
}

rows <- rbindlist(list(
  section_row(1, "Demographics"),
  cont_row(2, "Age, years", "age_at_icu", digits = 0),
  cat_row(3, "Male sex", "sex_factor", "M"),
  section_row(10, "Race"),
  cat_row(11, "White", "race_group", "WHITE"),
  cat_row(12, "Black", "race_group", "BLACK"),
  cat_row(13, "Asian", "race_group", "ASIAN"),
  cat_row(14, "Hispanic", "race_group", "HISPANIC"),
  cat_row(15, "Other race", "race_group", "OTHER"),
  cat_row(16, "Unknown race", "race_group", "UNKNOWN"),
  section_row(20, "Admission type"),
  cat_row(21, "Emergency", "admission_type_group", "EMERGENCY"),
  cat_row(22, "Urgent", "admission_type_group", "URGENT"),
  cat_row(23, "Elective surgical", "admission_type_group", "ELECTIVE_SURGICAL"),
  cat_row(24, "Observation", "admission_type_group", "OBSERVATION"),
  section_row(30, "Baseline severity"),
  cont_row(31, "Charlson comorbidity index", "charlson_comorbidity_index", digits = 0),
  cont_row(32, "SOFA score at t0", "sofa_total_t0_clean", digits = 0),
  cat_row(33, "SOFA missing/invalid", "sofa_total_t0_missing_or_invalid", "1"),
  cat_row(34, "Shock at t0", "shock_t0_b_num", "1"),
  section_row(40, "Auxiliary covariates"),
  cont_row(41, "Minimum mean BP, mmHg", "meanbp_min_t0_clean", digits = 0),
  cat_row(42, "Mean BP missing", "meanbp_min_t0_missing", "1"),
  cat_row(43, "Mean BP extreme/invalid", "meanbp_min_t0_extreme_or_invalid", "1")
), fill = TRUE)

setorder(rows, row_id)
rows[, row_id := NULL]

long <- melt(
  rows[row_type == "data"],
  id.vars = c("row_type", "section", "characteristic", "max_smd"),
  measure.vars = c("overall", "window_0_3h", "window_3_6h", "window_6_12h", "window_ge12h"),
  variable.name = "column",
  value.name = "value"
)

fwrite(rows, out_table, bom = TRUE)
fwrite(long, out_long, bom = TRUE)

cat("=== formal_v1 original analyzable cohort baseline table ===\n", file = out_report)
cat("sample: patient-level analyzable cohort before cloning, artificial censoring, or weighting\n", file = out_report, append = TRUE)
cat("source: formal_v1_analysis_main.csv, not MICE-imputed table\n", file = out_report, append = TRUE)
cat("continuous variables: median [IQR] among observed/clean values\n", file = out_report, append = TRUE)
cat("categorical variables: n (%)\n", file = out_report, append = TRUE)
cat("SMD: maximum absolute pairwise SMD across observed antibiotic initiation windows\n\n", file = out_report, append = TRUE)
cat("window counts:\n", file = out_report, append = TRUE)
capture.output(print(observed_counts), file = out_report, append = TRUE)
cat("\nmissing/extreme checks:\n", file = out_report, append = TRUE)
missing_checks <- cohort[, .(
  sofa_missing_invalid = sum(sofa_total_t0_missing_or_invalid == 1L, na.rm = TRUE),
  meanbp_missing = sum(meanbp_min_t0_missing == 1L, na.rm = TRUE),
  meanbp_extreme_invalid = sum(meanbp_min_t0_extreme_or_invalid == 1L, na.rm = TRUE)
)]
capture.output(print(missing_checks), file = out_report, append = TRUE)
cat("\nmanuscript notes:\n", file = out_report, append = TRUE)
cat("- The >=12h group is retained to describe the complete analyzable cohort; primary causal contrasts focus on 0-3h, 3-6h, and 6-12h.\n", file = out_report, append = TRUE)
cat("- Admission type is strongly imbalanced before weighting (Emergency max SMD 0.825), motivating CCW/IPCW adjustment and the separate weighted balance table.\n", file = out_report, append = TRUE)
cat("- Mean BP missingness is calculated within the analyzable cohort in this table; full-cohort summaries use different denominators.\n", file = out_report, append = TRUE)
cat("\noutput:\n", file = out_report, append = TRUE)
cat(out_table, "\n", file = out_report, append = TRUE)
cat(out_long, "\n", file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

cat("Wrote original analyzable baseline table:", out_table, "\n")
cat("Wrote long table:", out_long, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: original analyzable baseline table generated.\n")
