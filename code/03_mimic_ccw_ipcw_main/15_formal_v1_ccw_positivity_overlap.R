# formal_v1 CCW positivity and overlap diagnostics
# Scope: arm-specific IPCW tails and baseline covariate common-support checks
# among strategy-adherent clones used by the outcome models.

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

weights_file <- file.path(base_dir, "formal_v1_ccw_ipcw_main_final_weights_m5.csv")
wide_imp_file <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_model_ready_long.csv")

out_weight_detail <- file.path(base_dir, "formal_v1_ccw_positivity_weight_distribution_by_arm_m5.csv")
out_weight_summary <- file.path(base_dir, "formal_v1_ccw_positivity_weight_distribution_summary.csv")
out_cont_overlap <- file.path(base_dir, "formal_v1_ccw_positivity_continuous_overlap_m5.csv")
out_cat_support <- file.path(base_dir, "formal_v1_ccw_positivity_categorical_support_m5.csv")
out_flag_summary <- file.path(base_dir, "formal_v1_ccw_positivity_flag_summary.csv")
out_report <- file.path(base_dir, "formal_v1_ccw_positivity_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

wtd_mean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

summarise_weights <- function(dt, weight_col, label) {
  qfun <- function(x, p) as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 2))
  dt[!is.na(get(weight_col)), .(
    n_clones = .N,
    n_stays = uniqueN(stay_id),
    weight_sum = sum(get(weight_col), na.rm = TRUE),
    ess = (sum(get(weight_col), na.rm = TRUE)^2) / sum(get(weight_col)^2, na.rm = TRUE),
    min = min(get(weight_col), na.rm = TRUE),
    p01 = qfun(get(weight_col), 0.01),
    p05 = qfun(get(weight_col), 0.05),
    p25 = qfun(get(weight_col), 0.25),
    median = qfun(get(weight_col), 0.50),
    p75 = qfun(get(weight_col), 0.75),
    p95 = qfun(get(weight_col), 0.95),
    p99 = qfun(get(weight_col), 0.99),
    max = max(get(weight_col), na.rm = TRUE),
    n_gt_2 = sum(get(weight_col) > 2, na.rm = TRUE),
    pct_gt_2 = mean(get(weight_col) > 2, na.rm = TRUE),
    n_gt_5 = sum(get(weight_col) > 5, na.rm = TRUE),
    pct_gt_5 = mean(get(weight_col) > 5, na.rm = TRUE),
    n_gt_10 = sum(get(weight_col) > 10, na.rm = TRUE),
    pct_gt_10 = mean(get(weight_col) > 10, na.rm = TRUE)
  ), by = .(.imp, arm, arm_id)][, weight_type := label][]
}

continuous_overlap <- function(dt, variable, label, weight_col = "final_ipcw") {
  qfun <- function(x, p) as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 2))
  x <- as_num(dt[[variable]])
  tmp <- copy(dt)
  tmp[, x := x]

  by_arm <- tmp[!is.na(x), .(
    n = .N,
    weighted_mean = wtd_mean(x, get(weight_col)),
    min = min(x, na.rm = TRUE),
    p01 = qfun(x, 0.01),
    p05 = qfun(x, 0.05),
    p95 = qfun(x, 0.95),
    p99 = qfun(x, 0.99),
    max = max(x, na.rm = TRUE)
  ), by = .(.imp, arm, arm_id)]

  by_arm[, `:=`(
    variable = variable,
    variable_label = label
  )]

  common <- by_arm[, .(
    common_p01_low = max(p01, na.rm = TRUE),
    common_p99_high = min(p99, na.rm = TRUE),
    common_p05_low = max(p05, na.rm = TRUE),
    common_p95_high = min(p95, na.rm = TRUE)
  ), by = .imp]

  tmp <- merge(tmp, common, by = ".imp", all.x = TRUE, sort = FALSE)
  outside <- tmp[!is.na(x), .(
    n_overlap_eval = .N,
    outside_common_p01_p99_n = sum(x < common_p01_low | x > common_p99_high, na.rm = TRUE),
    outside_common_p01_p99_pct = mean(x < common_p01_low | x > common_p99_high, na.rm = TRUE),
    outside_common_p05_p95_n = sum(x < common_p05_low | x > common_p95_high, na.rm = TRUE),
    outside_common_p05_p95_pct = mean(x < common_p05_low | x > common_p95_high, na.rm = TRUE)
  ), by = .(.imp, arm, arm_id)]

  out <- merge(by_arm, common, by = ".imp", all.x = TRUE, sort = FALSE)
  out <- merge(out, outside, by = c(".imp", "arm", "arm_id"), all.x = TRUE, sort = FALSE)
  setcolorder(out, c(
    ".imp", "arm", "arm_id", "variable", "variable_label",
    "n", "weighted_mean", "min", "p01", "p05", "p95", "p99", "max",
    "common_p01_low", "common_p99_high", "n_overlap_eval", "outside_common_p01_p99_n",
    "outside_common_p01_p99_pct", "common_p05_low", "common_p95_high",
    "outside_common_p05_p95_n", "outside_common_p05_p95_pct"
  ))
  out
}

categorical_support <- function(dt, variable, label, weight_col = "final_ipcw") {
  levels_all <- sort(unique(as.character(dt[[variable]])))
  out <- rbindlist(lapply(levels_all, function(ll) {
    tmp <- dt[, .(
      n_level = sum(as.character(get(variable)) == ll, na.rm = TRUE),
      pct_level = mean(as.character(get(variable)) == ll, na.rm = TRUE),
      weighted_pct_level = sum(get(weight_col) * (as.character(get(variable)) == ll), na.rm = TRUE) /
        sum(get(weight_col), na.rm = TRUE)
    ), by = .(.imp, arm, arm_id)]
    tmp[, `:=`(
      variable = variable,
      variable_label = label,
      level = ll
    )]
    tmp
  }), fill = TRUE)

  support <- out[, .(
    min_n_across_arms = min(n_level, na.rm = TRUE),
    min_weighted_pct_across_arms = min(weighted_pct_level, na.rm = TRUE),
    zero_count_arm_n = sum(n_level == 0, na.rm = TRUE)
  ), by = .(.imp, variable, variable_label, level)]

  out <- merge(out, support, by = c(".imp", "variable", "variable_label", "level"), all.x = TRUE, sort = FALSE)
  setorder(out, .imp, variable, level, arm_id)
  out
}

stop_if_missing(weights_file, "Clone-level IPCW file")
stop_if_missing(wide_imp_file, "Imputed wide table")

cat("formal_v1 CCW positivity and overlap diagnostics\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Clone-level IPCW:", weights_file, "\n")
cat("Imputed wide table:", wide_imp_file, "\n")

need_weights <- c(
  ".imp", "clone_id", "stay_id", "arm", "arm_id", "strategy_adherent_by_delay",
  "death_30d", "final_ipcw", "final_ipcw_trunc_p01_p99", "final_ipcw_trunc_p05_p95"
)
need_wide <- c(
  ".imp", "stay_id", "age_at_icu_model", "sex_factor", "race_group",
  "admission_type_group", "charlson_comorbidity_index_model",
  "sofa_total_t0_imp", "sofa_total_t0_missing", "shock_t0_b_num_model"
)

weights_header <- names(fread(weights_file, nrows = 0, showProgress = FALSE))
wide_header <- names(fread(wide_imp_file, nrows = 0, showProgress = FALSE))
missing_weights <- setdiff(need_weights, weights_header)
missing_wide <- setdiff(need_wide, wide_header)
if (length(missing_weights) > 0) stop("Weights missing columns: ", paste(missing_weights, collapse = ", "), call. = FALSE)
if (length(missing_wide) > 0) stop("Wide table missing columns: ", paste(missing_wide, collapse = ", "), call. = FALSE)

weights <- fread(weights_file, select = need_weights, showProgress = FALSE)
wide <- fread(wide_imp_file, select = need_wide, showProgress = FALSE)

weights[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  arm_id = as.integer(arm_id),
  strategy_adherent_by_delay = as.logical(strategy_adherent_by_delay),
  final_ipcw = as_num(final_ipcw),
  final_ipcw_trunc_p01_p99 = as_num(final_ipcw_trunc_p01_p99),
  final_ipcw_trunc_p05_p95 = as_num(final_ipcw_trunc_p05_p95)
)]
wide[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  age_at_icu_model = as_num(age_at_icu_model),
  charlson_comorbidity_index_model = as_num(charlson_comorbidity_index_model),
  sofa_total_t0_imp = as_num(sofa_total_t0_imp),
  sofa_total_t0_missing = as.integer(as_num(sofa_total_t0_missing)),
  shock_t0_b_num_model = as.integer(as_num(shock_t0_b_num_model))
)]

weights <- weights[.imp %in% 1:5]
wide <- wide[.imp %in% 1:5]
if (weights[, anyDuplicated(paste(.imp, clone_id))] > 0) {
  stop("Duplicate .imp + clone_id rows in clone-level weights.", call. = FALSE)
}
if (wide[, anyDuplicated(paste(.imp, stay_id))] > 0) {
  stop("Duplicate .imp + stay_id rows in imputed wide table.", call. = FALSE)
}

dat <- merge(weights, wide, by = c(".imp", "stay_id"), all.x = TRUE, sort = FALSE)
adherent <- dat[strategy_adherent_by_delay == TRUE]
if (adherent[, any(is.na(final_ipcw))]) {
  stop("Some strategy-adherent clones have missing final_ipcw.", call. = FALSE)
}

main_covars <- c(
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model", "sofa_total_t0_imp",
  "sofa_total_t0_missing", "shock_t0_b_num_model"
)
if (adherent[, any(rowSums(is.na(.SD)) > 0), .SDcols = main_covars]) {
  stop("Main covariates contain NA among adherent clones.", call. = FALSE)
}

print_section("1. Arm Counts")
arm_counts <- adherent[, .(
  adherent_clones = .N,
  stays = uniqueN(stay_id),
  events_30d = sum(as_num(death_30d), na.rm = TRUE)
), by = .(.imp, arm, arm_id)]
setorder(arm_counts, .imp, arm_id)
print(arm_counts)

print_section("2. Weight Distribution by Arm")
weight_detail <- rbindlist(list(
  summarise_weights(adherent, "final_ipcw", "final_ipcw"),
  summarise_weights(adherent, "final_ipcw_trunc_p01_p99", "trunc_p01_p99"),
  summarise_weights(adherent, "final_ipcw_trunc_p05_p95", "trunc_p05_p95")
), fill = TRUE)
setorder(weight_detail, weight_type, .imp, arm_id)

weight_summary <- weight_detail[, .(
  n_arm_imp = .N,
  min_n_clones = min(n_clones, na.rm = TRUE),
  min_ess = min(ess, na.rm = TRUE),
  max_p95 = max(p95, na.rm = TRUE),
  max_p99 = max(p99, na.rm = TRUE),
  max_max = max(max, na.rm = TRUE),
  max_pct_gt_2 = max(pct_gt_2, na.rm = TRUE),
  max_pct_gt_5 = max(pct_gt_5, na.rm = TRUE),
  max_pct_gt_10 = max(pct_gt_10, na.rm = TRUE)
), by = weight_type]
print(weight_summary)

print_section("3. Continuous Covariate Overlap")
cont_specs <- data.table(
  variable = c("age_at_icu_model", "charlson_comorbidity_index_model", "sofa_total_t0_imp"),
  label = c("Age", "Charlson comorbidity index", "Baseline SOFA")
)
cont_overlap <- rbindlist(lapply(seq_len(nrow(cont_specs)), function(i) {
  continuous_overlap(adherent, cont_specs$variable[i], cont_specs$label[i], "final_ipcw")
}), fill = TRUE)
cont_summary <- cont_overlap[, .(
  max_outside_p01_p99_pct = max(outside_common_p01_p99_pct, na.rm = TRUE),
  max_outside_p05_p95_pct = max(outside_common_p05_p95_pct, na.rm = TRUE),
  min_common_p01_width = min(common_p99_high - common_p01_low, na.rm = TRUE),
  min_common_p05_width = min(common_p95_high - common_p05_low, na.rm = TRUE)
), by = variable_label]
print(cont_summary)

print_section("4. Categorical Support")
cat_specs <- data.table(
  variable = c("sex_factor", "race_group", "admission_type_group", "sofa_total_t0_missing", "shock_t0_b_num_model"),
  label = c("Sex", "Race group", "Admission type", "Baseline SOFA missing", "Baseline shock")
)
cat_support <- rbindlist(lapply(seq_len(nrow(cat_specs)), function(i) {
  categorical_support(adherent, cat_specs$variable[i], cat_specs$label[i], "final_ipcw")
}), fill = TRUE)
cat_summary <- cat_support[, .(
  min_n_across_arms_overall = min(min_n_across_arms, na.rm = TRUE),
  min_weighted_pct_across_arms_overall = min(min_weighted_pct_across_arms, na.rm = TRUE),
  n_imp_with_zero_count_arm = uniqueN(.imp[zero_count_arm_n > 0])
), by = .(variable, variable_label, level)]
setorder(cat_summary, variable, min_n_across_arms_overall)
print(cat_summary)

flag_summary <- data.table(
  check = c(
    "final_ipcw_max_gt_10",
    "final_ipcw_p99_gt_5",
    "final_ipcw_pct_gt_5_gt_1pct",
    "continuous_common_p01_p99_width_nonpositive",
    "main_categorical_zero_count_arm"
  ),
  value = c(
    weight_summary[weight_type == "final_ipcw", max_max] > 10,
    weight_summary[weight_type == "final_ipcw", max_p99] > 5,
    weight_summary[weight_type == "final_ipcw", max_pct_gt_5] > 0.01,
    cont_summary[, any(min_common_p01_width <= 0 | !is.finite(min_common_p01_width))],
    cat_summary[, any(n_imp_with_zero_count_arm > 0)]
  )
)
flag_summary[, status := fifelse(value, "flag", "pass")]

print_section("5. Flag Summary")
print(flag_summary)

fwrite(weight_detail, out_weight_detail)
fwrite(weight_summary, out_weight_summary)
fwrite(cont_overlap, out_cont_overlap)
fwrite(cat_support, out_cat_support)
fwrite(flag_summary, out_flag_summary)

cat("=== formal_v1 CCW positivity and overlap diagnostics ===\n", file = out_report)
cat("clone_weights:", weights_file, "\n", file = out_report, append = TRUE)
cat("wide_imputed:", wide_imp_file, "\n", file = out_report, append = TRUE)
cat("sample: strategy-adherent clones only.\n\n", file = out_report, append = TRUE)

cat("arm_counts:\n", file = out_report, append = TRUE)
capture.output(print(arm_counts), file = out_report, append = TRUE)
cat("\nweight_summary:\n", file = out_report, append = TRUE)
capture.output(print(weight_summary), file = out_report, append = TRUE)
cat("\ncontinuous_overlap_summary:\n", file = out_report, append = TRUE)
capture.output(print(cont_summary), file = out_report, append = TRUE)
cat("\ncategorical_support_summary:\n", file = out_report, append = TRUE)
capture.output(print(cat_summary), file = out_report, append = TRUE)
cat("\nflag_summary:\n", file = out_report, append = TRUE)
capture.output(print(flag_summary), file = out_report, append = TRUE)
cat("\noutputs:\n", file = out_report, append = TRUE)
cat(out_weight_detail, "\n", file = out_report, append = TRUE)
cat(out_weight_summary, "\n", file = out_report, append = TRUE)
cat(out_cont_overlap, "\n", file = out_report, append = TRUE)
cat(out_cat_support, "\n", file = out_report, append = TRUE)
cat(out_flag_summary, "\n", file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

print_section("6. Output")
cat("Wrote weight detail:", out_weight_detail, "\n")
cat("Wrote weight summary:", out_weight_summary, "\n")
cat("Wrote continuous overlap:", out_cont_overlap, "\n")
cat("Wrote categorical support:", out_cat_support, "\n")
cat("Wrote flag summary:", out_flag_summary, "\n")
cat("Wrote report:", out_report, "\n")
if (flag_summary[, any(status == "flag")]) {
  cat("WARNING: positivity/overlap flags require review.\n")
} else {
  cat("PASS: no positivity/overlap flags under prespecified simple rules.\n")
}
