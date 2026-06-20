set.seed(2025)

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

library(data.table)

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1) {
  args[1]
} else {
  "D:/codex 工作目录/TTE目标模拟实验/TTE正式实验/posthoc_ccw_enhancement_20260617"
}

analyses <- data.table(
  analysis_id = c("A3_cut_0_2_6_12", "A3_cut_0_4_8_12"),
  subgroup = c(
    "alternative cutpoints 0-2/2-6/6-12",
    "alternative cutpoints 0-4/4-8/8-12"
  )
)

read_one <- function(aid, file) {
  fread(file.path(root, aid, file), showProgress = FALSE)
}

contrast <- rbindlist(lapply(seq_len(nrow(analyses)), function(i) {
  x <- read_one(analyses$analysis_id[i], "formal_v1_ccw_30d_pooled_logistic_main_contrasts_mi.csv")
  x <- x[section == "pooled"]
  x[, `:=`(analysis_id = analyses$analysis_id[i], subgroup = analyses$subgroup[i])]
  x[, .(analysis_id, subgroup, contrast, or, or_ci_low, or_ci_high, p_value, estimate, robust_se)]
}), fill = TRUE)
fwrite(contrast, file.path(root, "posthoc_A3_cutpoint_contrast_summary.csv"))

risk <- rbindlist(lapply(seq_len(nrow(analyses)), function(i) {
  x <- read_one(analyses$analysis_id[i], "formal_v1_ccw_30d_pooled_logistic_main_risk_mi.csv")
  x <- x[section == "summary"]
  x[, `:=`(analysis_id = analyses$analysis_id[i], subgroup = analyses$subgroup[i])]
  x[, .(
    analysis_id,
    subgroup,
    arm,
    cumulative_risk_30d,
    reference_arm,
    risk_difference_vs_reference,
    risk_ratio_vs_reference
  )]
}), fill = TRUE)
fwrite(risk, file.path(root, "posthoc_A3_cutpoint_risk_summary.csv"))

weight_event <- rbindlist(lapply(seq_len(nrow(analyses)), function(i) {
  aid <- analyses$analysis_id[i]
  w <- read_one(aid, "formal_v1_ccw_ipcw_main_final_weights_m5.csv")
  w <- w[strategy_adherent_by_delay == TRUE]
  w[, final_ipcw := as.numeric(final_ipcw)]
  ess <- w[, .(
    n = .N,
    mean_weight = mean(final_ipcw),
    sd_weight = stats::sd(final_ipcw),
    max_weight = max(final_ipcw),
    gt10 = sum(final_ipcw > 10),
    gt20 = sum(final_ipcw > 20),
    gt50 = sum(final_ipcw > 50),
    ess = sum(final_ipcw)^2 / sum(final_ipcw^2),
    ess_pct = 100 * (sum(final_ipcw)^2 / sum(final_ipcw^2)) / .N
  ), by = .(.imp, arm, arm_id)]

  ppd <- read_one(aid, "formal_v1_ccw_30d_person_period_diagnostics.csv")
  ev <- ppd[section == "by_arm", .(
    events = period_event_sum,
    weighted_events = weighted_event_sum
  ), by = .(.imp, arm, arm_id)]

  out <- merge(ess, ev, by = c(".imp", "arm", "arm_id"), all.x = TRUE)
  out[, `:=`(analysis_id = aid, subgroup = analyses$subgroup[i])]
  out[, .(
    n = mean(n),
    mean_weight = mean(mean_weight),
    sd_weight = mean(sd_weight),
    max_weight = max(max_weight),
    gt10 = sum(gt10),
    gt20 = sum(gt20),
    gt50 = sum(gt50),
    ess = mean(ess),
    ess_pct = mean(ess_pct),
    events = mean(events),
    weighted_events = mean(weighted_events)
  ), by = .(analysis_id, subgroup, arm, arm_id)]
}), fill = TRUE)
fwrite(weight_event, file.path(root, "posthoc_A3_cutpoint_weight_event_ess_summary.csv"))

stability <- rbindlist(lapply(seq_len(nrow(analyses)), function(i) {
  aid <- analyses$analysis_id[i]
  w <- weight_event[analysis_id == aid]
  b <- read_one(aid, "formal_v1_ccw_balance_smd_summary.csv")
  max_smd <- b[
    sample == "adherent_only" &
      weighting == "ipcw_weighted_adherent" &
      variable_role == "main",
    max(max_abs_smd, na.rm = TRUE)
  ]
  data.table(
    analysis_id = aid,
    subgroup = analyses$subgroup[i],
    max_weight = max(w$max_weight, na.rm = TRUE),
    gt10_total = sum(w$gt10, na.rm = TRUE),
    min_ess_pct = min(w$ess_pct, na.rm = TRUE),
    min_events = min(w$events, na.rm = TRUE),
    max_weighted_smd = max_smd
  )
}), fill = TRUE)
stability[, stable_flag := fifelse(
  max_weight <= 10 & gt10_total == 0 & min_ess_pct >= 50 & max_weighted_smd <= 0.10,
  "STABLE",
  "DESCRIPTIVE-ONLY"
)]
fwrite(stability, file.path(root, "posthoc_A3_cutpoint_stability_summary.csv"))

old_files <- c(
  "posthoc_A1_A2s_A2f_contrast_summary.csv",
  "posthoc_A1_A2s_A2f_risk_summary.csv",
  "posthoc_A1_A2s_A2f_weight_event_ess_summary.csv",
  "posthoc_A1_A2s_A2f_stability_summary.csv"
)
if (all(file.exists(file.path(root, old_files)))) {
  old_contrast <- fread(file.path(root, old_files[1]))
  old_risk <- fread(file.path(root, old_files[2]))
  old_weight <- fread(file.path(root, old_files[3]))
  old_stability <- fread(file.path(root, old_files[4]))

  fwrite(rbindlist(list(old_contrast, contrast), fill = TRUE), file.path(root, "posthoc_A1_A2_A3_contrast_summary.csv"))
  fwrite(rbindlist(list(old_risk, risk), fill = TRUE), file.path(root, "posthoc_A1_A2_A3_risk_summary.csv"))
  fwrite(rbindlist(list(old_weight, weight_event), fill = TRUE), file.path(root, "posthoc_A1_A2_A3_weight_event_ess_summary.csv"))
  fwrite(rbindlist(list(old_stability, stability), fill = TRUE), file.path(root, "posthoc_A1_A2_A3_stability_summary.csv"))
}

cat("A3 contrast summary\n")
print(contrast)
cat("\nA3 risk summary\n")
print(risk)
cat("\nA3 stability summary\n")
print(stability)
cat("\nWrote A3 and combined A1-A3 summary files under:", root, "\n")
