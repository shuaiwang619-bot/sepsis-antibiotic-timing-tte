# formal_v1 CCW covariate balance diagnostics
# Scope: baseline covariate SMD before and after IPCW among strategy-adherent
# clones, plus an all-clones baseline sanity check.

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

out_detail <- file.path(base_dir, "formal_v1_ccw_balance_smd_detail_m5.csv")
out_summary <- file.path(base_dir, "formal_v1_ccw_balance_smd_summary.csv")
out_love <- file.path(base_dir, "formal_v1_ccw_balance_love_plot_data.csv")
out_plot <- file.path(base_dir, "formal_v1_ccw_balance_love_plot.png")
out_report <- file.path(base_dir, "formal_v1_ccw_balance_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

library(data.table)

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

weighted_mean_var <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (length(x) == 0L || sum(w) <= 0) {
    return(c(mean = NA_real_, var = NA_real_, n = 0, wsum = NA_real_))
  }
  mu <- sum(w * x) / sum(w)
  vv <- sum(w * (x - mu)^2) / sum(w)
  c(mean = mu, var = vv, n = length(x), wsum = sum(w))
}

smd_two_groups <- function(m1, v1, m2, v2) {
  denom <- sqrt((v1 + v2) / 2)
  if (!is.finite(denom) || denom < 1e-12) {
    if (is.finite(m1) && is.finite(m2) && abs(m1 - m2) < 1e-12) return(0)
    return(NA_real_)
  }
  (m2 - m1) / denom
}

make_numeric_value <- function(dat, variable_name, type, level = NA_character_) {
  x <- dat[[variable_name]]
  if (type == "continuous") return(as_num(x))
  if (type == "binary") return(as_num(x))
  as.integer(as.character(x) == level)
}

compute_balance <- function(dat, variable_spec, weight_col, weighting, sample_label) {
  arm_levels <- c("0-3h", "3-6h", "6-12h")
  contrast_def <- data.table(
    contrast = c("3-6h_vs_0-3h", "6-12h_vs_0-3h", "6-12h_vs_3-6h"),
    arm_ref = c("0-3h", "0-3h", "3-6h"),
    arm_comp = c("3-6h", "6-12h", "6-12h")
  )

  out <- vector("list", nrow(variable_spec) * length(unique(dat$.imp)) * nrow(contrast_def))
  k <- 0L

  for (ii in sort(unique(dat$.imp))) {
    di <- dat[.imp == ii]
    for (jj in seq_len(nrow(variable_spec))) {
      spec <- variable_spec[jj]
      x <- make_numeric_value(di, spec$variable, spec$type, spec$level)
      tmp <- data.table(
        arm = as.character(di$arm),
        x = x,
        w = as_num(di[[weight_col]])
      )
      stats <- tmp[arm %in% arm_levels, as.list(weighted_mean_var(x, w)), by = arm]

      for (cc in seq_len(nrow(contrast_def))) {
        ref <- stats[arm == contrast_def$arm_ref[cc]]
        comp <- stats[arm == contrast_def$arm_comp[cc]]
        k <- k + 1L
        out[[k]] <- data.table(
          .imp = ii,
          sample = sample_label,
          weighting = weighting,
          variable = spec$variable,
          variable_label = spec$label,
          variable_role = spec$role,
          variable_type = spec$type,
          level = spec$level,
          display_label = spec$display_label,
          contrast = contrast_def$contrast[cc],
          arm_ref = contrast_def$arm_ref[cc],
          arm_comp = contrast_def$arm_comp[cc],
          ref_mean = ref$mean,
          comp_mean = comp$mean,
          ref_var = ref$var,
          comp_var = comp$var,
          ref_n = ref$n,
          comp_n = comp$n,
          ref_wsum = ref$wsum,
          comp_wsum = comp$wsum,
          smd = smd_two_groups(ref$mean, ref$var, comp$mean, comp$var),
          abs_smd = abs(smd_two_groups(ref$mean, ref$var, comp$mean, comp$var))
        )
      }
    }
  }

  rbindlist(out[seq_len(k)], fill = TRUE)
}

stop_if_missing(weights_file, "Clone-level IPCW file")
stop_if_missing(wide_imp_file, "Imputed wide table")

cat("formal_v1 CCW covariate balance diagnostics\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Clone-level IPCW:", weights_file, "\n")
cat("Imputed wide table:", wide_imp_file, "\n")

need_weights <- c(
  ".imp", "clone_id", "stay_id", "arm", "arm_id",
  "strategy_adherent_by_delay",
  "final_ipcw", "final_ipcw_trunc_p01_p99", "final_ipcw_trunc_p05_p95"
)
need_wide <- c(
  ".imp", "stay_id",
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model",
  "sofa_total_t0_missing", "shock_t0_b_factor", "shock_t0_b_num_model",
  "meanbp_min_t0_missing", "meanbp_min_t0_extreme_or_invalid",
  "sofa_total_t0_imp", "meanbp_min_t0_imp_sens"
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
  sofa_total_t0_missing = as.integer(as_num(sofa_total_t0_missing)),
  shock_t0_b_num_model = as.integer(as_num(shock_t0_b_num_model)),
  meanbp_min_t0_missing = as.integer(as_num(meanbp_min_t0_missing)),
  meanbp_min_t0_extreme_or_invalid = as.integer(as_num(meanbp_min_t0_extreme_or_invalid)),
  sofa_total_t0_imp = as_num(sofa_total_t0_imp),
  meanbp_min_t0_imp_sens = as_num(meanbp_min_t0_imp_sens)
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
dat[, unweighted := 1]

main_needed <- c(
  "age_at_icu_model", "sex_factor", "race_group", "admission_type_group",
  "charlson_comorbidity_index_model", "sofa_total_t0_imp",
  "sofa_total_t0_missing", "shock_t0_b_num_model"
)
if (dat[, any(rowSums(is.na(.SD)) > 0), .SDcols = main_needed]) {
  stop("Main balance covariates contain NA.", call. = FALSE)
}

continuous_spec <- data.table(
  variable = c(
    "age_at_icu_model",
    "charlson_comorbidity_index_model",
    "sofa_total_t0_imp",
    "meanbp_min_t0_imp_sens"
  ),
  label = c(
    "Age",
    "Charlson comorbidity index",
    "Baseline SOFA",
    "Baseline mean BP"
  ),
  role = c("main", "main", "main", "auxiliary"),
  type = "continuous",
  level = NA_character_
)
continuous_spec[, display_label := label]

binary_spec <- data.table(
  variable = c(
    "sofa_total_t0_missing",
    "shock_t0_b_num_model",
    "meanbp_min_t0_missing",
    "meanbp_min_t0_extreme_or_invalid"
  ),
  label = c(
    "Baseline SOFA missing",
    "Baseline shock",
    "Baseline mean BP missing",
    "Baseline mean BP extreme/invalid"
  ),
  role = c("main", "main", "auxiliary", "auxiliary"),
  type = "binary",
  level = NA_character_
)
binary_spec[, display_label := label]

categorical_vars <- data.table(
  variable = c("sex_factor", "race_group", "admission_type_group"),
  label = c("Sex", "Race group", "Admission type"),
  role = c("main", "main", "main")
)
cat_spec <- rbindlist(lapply(seq_len(nrow(categorical_vars)), function(i) {
  vv <- categorical_vars$variable[i]
  lev <- sort(unique(as.character(dat[[vv]])))
  data.table(
    variable = vv,
    label = categorical_vars$label[i],
    role = categorical_vars$role[i],
    type = "categorical",
    level = lev,
    display_label = paste0(categorical_vars$label[i], ": ", lev)
  )
}), fill = TRUE)

variable_spec <- rbindlist(list(continuous_spec, binary_spec, cat_spec), fill = TRUE)

adherent <- dat[strategy_adherent_by_delay == TRUE]
if (adherent[, any(is.na(final_ipcw))]) {
  stop("Some strategy-adherent clones have missing final_ipcw.", call. = FALSE)
}

print_section("1. Input Counts")
counts <- rbindlist(list(
  dat[, .(
    clones = .N,
    stays = uniqueN(stay_id),
    weight_sum = sum(unweighted)
  ), by = .(.imp, arm, arm_id)][, sample := "all_clones_sanity"],
  adherent[, .(
    clones = .N,
    stays = uniqueN(stay_id),
    weight_sum = sum(final_ipcw)
  ), by = .(.imp, arm, arm_id)][, sample := "adherent_ipcw"]
), fill = TRUE)
setorder(counts, .imp, sample, arm_id)
print(counts)

print_section("2. Computing SMD")
detail <- rbindlist(list(
  compute_balance(dat, variable_spec, "unweighted", "unweighted_all_clones", "all_clones_sanity"),
  compute_balance(adherent, variable_spec, "unweighted", "unweighted_adherent", "adherent_only"),
  compute_balance(adherent, variable_spec, "final_ipcw", "ipcw_weighted_adherent", "adherent_only"),
  compute_balance(adherent, variable_spec, "final_ipcw_trunc_p01_p99", "ipcw_trunc_p01_p99", "adherent_only"),
  compute_balance(adherent, variable_spec, "final_ipcw_trunc_p05_p95", "ipcw_trunc_p05_p95", "adherent_only")
), fill = TRUE)

summary <- detail[, .(
  m = .N,
  mean_abs_smd = mean(abs_smd, na.rm = TRUE),
  max_abs_smd = max(abs_smd, na.rm = TRUE),
  n_gt_0_1 = sum(abs_smd > 0.1, na.rm = TRUE),
  n_gt_0_2 = sum(abs_smd > 0.2, na.rm = TRUE)
), by = .(sample, weighting, variable_role, display_label)]
setorder(summary, sample, weighting, variable_role, -max_abs_smd)

love <- summary[
  sample == "adherent_only" &
    weighting %in% c("unweighted_adherent", "ipcw_weighted_adherent") &
    variable_role == "main",
  .(
    display_label,
    weighting,
    mean_abs_smd,
    max_abs_smd,
    n_gt_0_1,
    n_gt_0_2
  )
]

main_after <- summary[
  sample == "adherent_only" &
    weighting == "ipcw_weighted_adherent" &
    variable_role == "main"
]
main_after_flag <- main_after[, .(
  variables = .N,
  max_abs_smd_overall = max(max_abs_smd, na.rm = TRUE),
  variables_gt_0_1 = sum(max_abs_smd > 0.1, na.rm = TRUE),
  variables_gt_0_2 = sum(max_abs_smd > 0.2, na.rm = TRUE)
)]

print_section("3. Main Covariate Balance After IPCW")
print(main_after[order(-max_abs_smd)])
print(main_after_flag)

fwrite(detail, out_detail)
fwrite(summary, out_summary)
fwrite(love, out_love)

plot_status <- "not_attempted"
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  plot_dat <- copy(love)
  plot_dat[, weighting_label := fifelse(
    weighting == "unweighted_adherent",
    "Unweighted adherent",
    "IPCW weighted adherent"
  )]
  order_labels <- plot_dat[weighting == "ipcw_weighted_adherent"][order(max_abs_smd), display_label]
  plot_dat[, display_label := factor(display_label, levels = unique(order_labels))]
  p <- ggplot(plot_dat, aes(x = max_abs_smd, y = display_label, color = weighting_label)) +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "grey45") +
    geom_point(size = 2.2, alpha = 0.9) +
    scale_color_manual(values = c("Unweighted adherent" = "#B00020", "IPCW weighted adherent" = "#005A8D")) +
    labs(
      x = "Maximum absolute pairwise SMD across arms",
      y = NULL,
      color = NULL,
      title = "formal_v1 baseline covariate balance"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  ggsave(out_plot, p, width = 8.5, height = 6.5, dpi = 160)
  plot_status <- "written"
} else {
  plot_status <- "ggplot2_not_available"
}

cat("=== formal_v1 CCW covariate balance diagnostics ===\n", file = out_report)
cat("clone_weights:", weights_file, "\n", file = out_report, append = TRUE)
cat("wide_imputed:", wide_imp_file, "\n", file = out_report, append = TRUE)
cat("balance samples:\n", file = out_report, append = TRUE)
cat("- all_clones_sanity: all cloned arms before artificial censoring; expected SMD is 0.\n", file = out_report, append = TRUE)
cat("- adherent_only: clones adherent to assigned strategy through the strategy window.\n", file = out_report, append = TRUE)
cat("primary diagnostic: adherent_only + ipcw_weighted_adherent.\n\n", file = out_report, append = TRUE)

cat("main_after_ipcw_flag:\n", file = out_report, append = TRUE)
capture.output(print(main_after_flag), file = out_report, append = TRUE)
cat("\nmain_after_ipcw_summary:\n", file = out_report, append = TRUE)
capture.output(print(main_after[order(-max_abs_smd)]), file = out_report, append = TRUE)
cat("\ncounts:\n", file = out_report, append = TRUE)
capture.output(print(counts), file = out_report, append = TRUE)
cat("\noutputs:\n", file = out_report, append = TRUE)
cat(out_detail, "\n", file = out_report, append = TRUE)
cat(out_summary, "\n", file = out_report, append = TRUE)
cat(out_love, "\n", file = out_report, append = TRUE)
cat(out_plot, " plot_status=", plot_status, "\n", sep = "", file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

print_section("4. Output")
cat("Wrote detail:", out_detail, "\n")
cat("Wrote summary:", out_summary, "\n")
cat("Wrote love plot data:", out_love, "\n")
cat("Plot status:", plot_status, "\n")
if (plot_status == "written") cat("Wrote love plot:", out_plot, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: balance diagnostics completed.\n")
