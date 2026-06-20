# formal_v1 Table 1 baseline characteristics
# Scope: strategy-adherent clones, before and after IPCW.
# Output: publication-style three-arm baseline table with maximum pairwise SMD.

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
smd_summary_file <- file.path(base_dir, "formal_v1_ccw_balance_smd_summary.csv")

out_table <- file.path(base_dir, "formal_v1_table1_baseline_characteristics_m5.csv")
out_long <- file.path(base_dir, "formal_v1_table1_baseline_characteristics_long_m5.csv")
out_report <- file.path(base_dir, "formal_v1_table1_baseline_characteristics_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
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

weighted_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0
  x <- as_num(x[ok])
  w <- as_num(w[ok])
  if (length(x) == 0L || sum(w) <= 0) return(rep(NA_real_, length(probs)))
  o <- order(x)
  x <- x[o]
  w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
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

format_cat <- function(n, pct, weighted = FALSE) {
  n_txt <- if (weighted) fmt_num(n, 1) else fmt_int(n)
  paste0(n_txt, " (", fmt_pct(pct, 1), ")")
}

stop_if_missing(weights_file, "Clone-level IPCW file")
stop_if_missing(wide_imp_file, "Imputed wide table")
stop_if_missing(smd_summary_file, "SMD summary file")

need_weights <- c(
  ".imp", "clone_id", "stay_id", "arm", "arm_id", "strategy_adherent_by_delay",
  "death_30d", "final_ipcw"
)
need_wide <- c(
  ".imp", "stay_id", "age_at_icu_model", "sex_factor", "race_group",
  "admission_type_group", "charlson_comorbidity_index_model",
  "sofa_total_t0_imp", "sofa_total_t0_missing", "shock_t0_b_num_model",
  "meanbp_min_t0_imp_sens", "meanbp_min_t0_missing",
  "meanbp_min_t0_extreme_or_invalid"
)

weights_header <- names(fread(weights_file, nrows = 0, showProgress = FALSE))
wide_header <- names(fread(wide_imp_file, nrows = 0, showProgress = FALSE))
missing_weights <- setdiff(need_weights, weights_header)
missing_wide <- setdiff(need_wide, wide_header)
if (length(missing_weights) > 0) stop("Weights missing columns: ", paste(missing_weights, collapse = ", "), call. = FALSE)
if (length(missing_wide) > 0) stop("Wide table missing columns: ", paste(missing_wide, collapse = ", "), call. = FALSE)

weights <- fread(weights_file, select = need_weights, showProgress = FALSE)
wide <- fread(wide_imp_file, select = need_wide, showProgress = FALSE)
smd <- fread(smd_summary_file, showProgress = FALSE)

weights[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  arm_id = as.integer(arm_id),
  strategy_adherent_by_delay = as.logical(strategy_adherent_by_delay),
  final_ipcw = as_num(final_ipcw)
)]
wide[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  age_at_icu_model = as_num(age_at_icu_model),
  charlson_comorbidity_index_model = as_num(charlson_comorbidity_index_model),
  sofa_total_t0_imp = as_num(sofa_total_t0_imp),
  sofa_total_t0_missing = as.integer(as_num(sofa_total_t0_missing)),
  shock_t0_b_num_model = as.integer(as_num(shock_t0_b_num_model)),
  meanbp_min_t0_imp_sens = as_num(meanbp_min_t0_imp_sens),
  meanbp_min_t0_missing = as.integer(as_num(meanbp_min_t0_missing)),
  meanbp_min_t0_extreme_or_invalid = as.integer(as_num(meanbp_min_t0_extreme_or_invalid))
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

arm_levels <- c("0-3h", "3-6h", "6-12h")
adherent[, arm := factor(as.character(arm), levels = arm_levels)]
adherent[, unweighted := 1]

counts <- adherent[, .(
  n = .N,
  w_n = sum(final_ipcw, na.rm = TRUE)
), by = .(.imp, arm, arm_id)]
counts_pool <- counts[, .(
  n = mean(n),
  w_n = mean(w_n)
), by = .(arm, arm_id)]
setorder(counts_pool, arm_id)

get_smd <- function(target_display_label, target_weighting) {
  ans <- smd[
    sample == "adherent_only" &
      weighting == target_weighting &
      display_label == target_display_label,
    max_abs_smd
  ]
  if (length(ans) == 0L) return(NA_real_)
  as_num(ans[1])
}

cont_row <- function(row_id, label, variable, smd_label, digits = 1, section = "") {
  per_imp <- adherent[, {
    x <- as_num(get(variable))
    uq <- plain_quantile(x, c(0.25, 0.50, 0.75))
    wq <- weighted_quantile(x, final_ipcw, c(0.25, 0.50, 0.75))
    .(
      un_q25 = uq[1], un_q50 = uq[2], un_q75 = uq[3],
      wt_q25 = wq[1], wt_q50 = wq[2], wt_q75 = wq[3]
    )
  }, by = .(.imp, arm, arm_id)]
  pooled <- per_imp[, lapply(.SD, mean, na.rm = TRUE), by = .(arm, arm_id),
                    .SDcols = c("un_q25", "un_q50", "un_q75", "wt_q25", "wt_q50", "wt_q75")]
  setorder(pooled, arm_id)

  data.table(
    row_id = row_id,
    row_type = "data",
    section = section,
    characteristic = label,
    unweighted_0_3h = format_cont(pooled[arm == "0-3h", un_q25], pooled[arm == "0-3h", un_q50], pooled[arm == "0-3h", un_q75], digits),
    unweighted_3_6h = format_cont(pooled[arm == "3-6h", un_q25], pooled[arm == "3-6h", un_q50], pooled[arm == "3-6h", un_q75], digits),
    unweighted_6_12h = format_cont(pooled[arm == "6-12h", un_q25], pooled[arm == "6-12h", un_q50], pooled[arm == "6-12h", un_q75], digits),
    max_smd_before = fmt_smd(get_smd(smd_label, "unweighted_adherent")),
    weighted_0_3h = format_cont(pooled[arm == "0-3h", wt_q25], pooled[arm == "0-3h", wt_q50], pooled[arm == "0-3h", wt_q75], digits),
    weighted_3_6h = format_cont(pooled[arm == "3-6h", wt_q25], pooled[arm == "3-6h", wt_q50], pooled[arm == "3-6h", wt_q75], digits),
    weighted_6_12h = format_cont(pooled[arm == "6-12h", wt_q25], pooled[arm == "6-12h", wt_q50], pooled[arm == "6-12h", wt_q75], digits),
    max_smd_after = fmt_smd(get_smd(smd_label, "ipcw_weighted_adherent"))
  )
}

cat_row <- function(row_id, label, variable, level, smd_label, section = "") {
  per_imp <- adherent[, {
    is_level <- as.character(get(variable)) == level
    n_arm <- .N
    w_sum <- sum(final_ipcw, na.rm = TRUE)
    .(
      un_n = sum(is_level, na.rm = TRUE),
      un_pct = sum(is_level, na.rm = TRUE) / n_arm,
      wt_n = sum(final_ipcw * is_level, na.rm = TRUE),
      wt_pct = sum(final_ipcw * is_level, na.rm = TRUE) / w_sum
    )
  }, by = .(.imp, arm, arm_id)]
  pooled <- per_imp[, lapply(.SD, mean, na.rm = TRUE), by = .(arm, arm_id),
                    .SDcols = c("un_n", "un_pct", "wt_n", "wt_pct")]
  setorder(pooled, arm_id)

  data.table(
    row_id = row_id,
    row_type = "data",
    section = section,
    characteristic = label,
    unweighted_0_3h = format_cat(pooled[arm == "0-3h", un_n], pooled[arm == "0-3h", un_pct], weighted = FALSE),
    unweighted_3_6h = format_cat(pooled[arm == "3-6h", un_n], pooled[arm == "3-6h", un_pct], weighted = FALSE),
    unweighted_6_12h = format_cat(pooled[arm == "6-12h", un_n], pooled[arm == "6-12h", un_pct], weighted = FALSE),
    max_smd_before = fmt_smd(get_smd(smd_label, "unweighted_adherent")),
    weighted_0_3h = format_cat(pooled[arm == "0-3h", wt_n], pooled[arm == "0-3h", wt_pct], weighted = TRUE),
    weighted_3_6h = format_cat(pooled[arm == "3-6h", wt_n], pooled[arm == "3-6h", wt_pct], weighted = TRUE),
    weighted_6_12h = format_cat(pooled[arm == "6-12h", wt_n], pooled[arm == "6-12h", wt_pct], weighted = TRUE),
    max_smd_after = fmt_smd(get_smd(smd_label, "ipcw_weighted_adherent"))
  )
}

section_row <- function(row_id, label) {
  data.table(
    row_id = row_id,
    row_type = "section",
    section = label,
    characteristic = label,
    unweighted_0_3h = "", unweighted_3_6h = "", unweighted_6_12h = "",
    max_smd_before = "", weighted_0_3h = "", weighted_3_6h = "",
    weighted_6_12h = "", max_smd_after = ""
  )
}

rows <- rbindlist(list(
  section_row(1, "Demographics"),
  cont_row(2, "Age, years", "age_at_icu_model", "Age", digits = 0),
  cat_row(3, "Male sex", "sex_factor", "M", "Sex: M"),
  section_row(10, "Race"),
  cat_row(11, "White", "race_group", "WHITE", "Race group: WHITE"),
  cat_row(12, "Black", "race_group", "BLACK", "Race group: BLACK"),
  cat_row(13, "Asian", "race_group", "ASIAN", "Race group: ASIAN"),
  cat_row(14, "Hispanic", "race_group", "HISPANIC", "Race group: HISPANIC"),
  cat_row(15, "Other race", "race_group", "OTHER", "Race group: OTHER"),
  cat_row(16, "Unknown race", "race_group", "UNKNOWN", "Race group: UNKNOWN"),
  section_row(20, "Admission type"),
  cat_row(21, "Emergency", "admission_type_group", "EMERGENCY", "Admission type: EMERGENCY"),
  cat_row(22, "Urgent", "admission_type_group", "URGENT", "Admission type: URGENT"),
  cat_row(23, "Elective surgical", "admission_type_group", "ELECTIVE_SURGICAL", "Admission type: ELECTIVE_SURGICAL"),
  cat_row(24, "Observation", "admission_type_group", "OBSERVATION", "Admission type: OBSERVATION"),
  section_row(30, "Baseline severity"),
  cont_row(31, "Charlson comorbidity index", "charlson_comorbidity_index_model", "Charlson comorbidity index", digits = 0),
  cont_row(32, "SOFA score at t0", "sofa_total_t0_imp", "Baseline SOFA", digits = 0),
  cat_row(33, "SOFA missing", "sofa_total_t0_missing", "1", "Baseline SOFA missing"),
  cat_row(34, "Shock at t0", "shock_t0_b_num_model", "1", "Baseline shock"),
  section_row(40, "Auxiliary covariates"),
  cont_row(41, "Minimum mean BP, mmHg", "meanbp_min_t0_imp_sens", "Baseline mean BP", digits = 0),
  cat_row(42, "Mean BP missing", "meanbp_min_t0_missing", "1", "Baseline mean BP missing"),
  cat_row(43, "Mean BP extreme/invalid", "meanbp_min_t0_extreme_or_invalid", "1", "Baseline mean BP extreme/invalid")
), fill = TRUE)

setorder(rows, row_id)
rows[, row_id := NULL]

header_counts <- counts_pool[, .(
  arm,
  n = fmt_int(n),
  weighted_n = fmt_num(w_n, 1)
)]

long <- melt(
  rows[row_type == "data"],
  id.vars = c("row_type", "section", "characteristic", "max_smd_before", "max_smd_after"),
  measure.vars = c(
    "unweighted_0_3h", "unweighted_3_6h", "unweighted_6_12h",
    "weighted_0_3h", "weighted_3_6h", "weighted_6_12h"
  ),
  variable.name = "column",
  value.name = "value"
)

fwrite(rows, out_table, bom = TRUE)
fwrite(long, out_long, bom = TRUE)

cat("=== formal_v1 Table 1 baseline characteristics ===\n", file = out_report)
cat("sample: strategy-adherent clones only\n", file = out_report, append = TRUE)
cat("mice imputations: m=5; per-imputation summaries averaged for display\n", file = out_report, append = TRUE)
cat("continuous variables: median [IQR]\n", file = out_report, append = TRUE)
cat("categorical variables: n (%) for unweighted; weighted n (%) for IPCW-weighted columns\n", file = out_report, append = TRUE)
cat("SMD: maximum absolute pairwise SMD across the three arms\n\n", file = out_report, append = TRUE)
cat("arm counts:\n", file = out_report, append = TRUE)
capture.output(print(header_counts), file = out_report, append = TRUE)
cat("\noutput:\n", file = out_report, append = TRUE)
cat(out_table, "\n", file = out_report, append = TRUE)
cat(out_long, "\n", file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

cat("Wrote Table 1:", out_table, "\n")
cat("Wrote long Table 1:", out_long, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: Table 1 generated.\n")
