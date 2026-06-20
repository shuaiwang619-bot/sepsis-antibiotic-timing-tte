# formal_v1 main results tables and forest plot
# Scope: summarize the locked main model (coarse-time pooled logistic) and
# sensitivity analyses for the 30-day mortality target trial emulation.

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

coarse_contrast_file <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_coarse_time_contrasts_mi.csv")
coarse_risk_file <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_coarse_time_risk_mi.csv")
full_contrast_file <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_main_contrasts_mi.csv")
cox_contrast_file <- file.path(base_dir, "formal_v1_ccw_30d_weighted_cox_sensitivity_contrasts_mi.csv")
trunc_contrast_file <- file.path(base_dir, "formal_v1_ccw_30d_pooled_logistic_truncated_weights_contrasts_mi.csv")

out_main <- file.path(base_dir, "formal_v1_results_main_model_table.csv")
out_sens <- file.path(base_dir, "formal_v1_results_sensitivity_table.csv")
out_forest_data <- file.path(base_dir, "formal_v1_results_forest_plot_data.csv")
out_forest_png <- file.path(base_dir, "formal_v1_results_forest_plot.png")
out_report <- file.path(base_dir, "formal_v1_results_tables_forest_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

fmt_num <- function(x, digits = 2) {
  ifelse(
    is.finite(x),
    formatC(x, format = "f", digits = digits, big.mark = ","),
    ""
  )
}

fmt_pct <- function(x, digits = 2) {
  ifelse(is.finite(x), paste0(formatC(100 * x, format = "f", digits = digits), "%"), "")
}

fmt_pp <- function(x, digits = 2) {
  ifelse(is.finite(x), paste0(formatC(100 * x, format = "f", digits = digits), " pp"), "")
}

fmt_p <- function(x) {
  ifelse(
    !is.finite(x), "",
    ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3))
  )
}

fmt_est_ci <- function(est, lo, hi, digits = 2) {
  paste0(fmt_num(est, digits), " (", fmt_num(lo, digits), "-", fmt_num(hi, digits), ")")
}

contrast_label <- function(x) {
  fcase(
    x == "3-6h_vs_0-3h", "3-6h vs 0-3h",
    x == "6-12h_vs_0-3h", "6-12h vs 0-3h",
    x == "6-12h_vs_3-6h", "6-12h vs 3-6h",
    default = x
  )
}

contrast_order <- function(x) {
  fcase(
    x == "3-6h_vs_0-3h", 1,
    x == "6-12h_vs_0-3h", 2,
    x == "6-12h_vs_3-6h", 3,
    default = 99
  )
}

read_pooled_or <- function(path, model_name, measure = "OR") {
  dat <- fread(path, showProgress = FALSE)
  dat <- dat[section == "pooled"]
  est_col <- if (measure == "HR") "hr" else "or"
  low_col <- if (measure == "HR") "hr_ci_low" else "or_ci_low"
  high_col <- if (measure == "HR") "hr_ci_high" else "or_ci_high"
  data.table(
    model = model_name,
    contrast = dat$contrast,
    contrast_label = contrast_label(dat$contrast),
    contrast_order = contrast_order(dat$contrast),
    measure = measure,
    estimate = as_num(dat[[est_col]]),
    ci_low = as_num(dat[[low_col]]),
    ci_high = as_num(dat[[high_col]]),
    p_value = as_num(dat$p_value)
  )
}

stop_if_missing(coarse_contrast_file, "Coarse-time contrast file")
stop_if_missing(coarse_risk_file, "Coarse-time risk file")
stop_if_missing(full_contrast_file, "Full time-axis contrast file")
stop_if_missing(cox_contrast_file, "Weighted Cox contrast file")
stop_if_missing(trunc_contrast_file, "Truncated weights contrast file")

main_effect <- read_pooled_or(coarse_contrast_file, "Main: coarse-time pooled logistic", "OR")

risk <- fread(coarse_risk_file, showProgress = FALSE)[section == "summary"]
risk[, `:=`(
  cumulative_risk_30d = as_num(cumulative_risk_30d),
  risk_difference_vs_0_3h = as_num(risk_difference_vs_0_3h),
  risk_ratio_vs_0_3h = as_num(risk_ratio_vs_0_3h)
)]
risk_lookup <- risk[, .(arm, cumulative_risk_30d)]

risk_for_contrast <- function(contrast) {
  if (contrast == "3-6h_vs_0-3h") return(c(ref = "0-3h", comp = "3-6h"))
  if (contrast == "6-12h_vs_0-3h") return(c(ref = "0-3h", comp = "6-12h"))
  if (contrast == "6-12h_vs_3-6h") return(c(ref = "3-6h", comp = "6-12h"))
  c(ref = NA_character_, comp = NA_character_)
}

main_rows <- rbindlist(lapply(seq_len(nrow(main_effect)), function(i) {
  cc <- main_effect$contrast[i]
  arms <- risk_for_contrast(cc)
  ref_risk <- risk_lookup[arm == arms["ref"], cumulative_risk_30d]
  comp_risk <- risk_lookup[arm == arms["comp"], cumulative_risk_30d]
  rd <- comp_risk - ref_risk
  rr <- comp_risk / ref_risk
  data.table(
    contrast = main_effect$contrast_label[i],
    reference_strategy = arms["ref"],
    comparison_strategy = arms["comp"],
    reference_30d_risk = fmt_pct(ref_risk),
    comparison_30d_risk = fmt_pct(comp_risk),
    risk_difference = fmt_pp(rd),
    risk_ratio = fmt_num(rr, 2),
    odds_ratio_95_ci = fmt_est_ci(main_effect$estimate[i], main_effect$ci_low[i], main_effect$ci_high[i]),
    p_value = fmt_p(main_effect$p_value[i])
  )
}), fill = TRUE)
main_rows[, order := contrast_order(gsub(" ", "", gsub(" vs ", "_vs_", contrast)))]
main_rows <- main_rows[order(match(contrast, c("3-6h vs 0-3h", "6-12h vs 0-3h", "6-12h vs 3-6h")))]
main_rows[, order := NULL]

full_effect <- read_pooled_or(full_contrast_file, "Sensitivity: full hourly time-axis pooled logistic", "OR")
cox_effect <- read_pooled_or(cox_contrast_file, "Sensitivity: weighted Cox model", "HR")

trunc_raw <- fread(trunc_contrast_file, showProgress = FALSE)[section == "pooled"]
trunc_effect <- rbindlist(list(
  trunc_raw[weight_scheme == "p01_p99", .(
    model = "Sensitivity: pooled logistic with p01-p99 weight truncation",
    contrast,
    contrast_label = contrast_label(contrast),
    contrast_order = contrast_order(contrast),
    measure = "OR",
    estimate = as_num(or),
    ci_low = as_num(or_ci_low),
    ci_high = as_num(or_ci_high),
    p_value = as_num(p_value)
  )],
  trunc_raw[weight_scheme == "p05_p95", .(
    model = "Sensitivity: pooled logistic with p05-p95 weight truncation",
    contrast,
    contrast_label = contrast_label(contrast),
    contrast_order = contrast_order(contrast),
    measure = "OR",
    estimate = as_num(or),
    ci_low = as_num(or_ci_low),
    ci_high = as_num(or_ci_high),
    p_value = as_num(p_value)
  )]
), fill = TRUE)

forest <- rbindlist(list(main_effect, full_effect, cox_effect, trunc_effect), fill = TRUE)
model_levels <- c(
  "Main: coarse-time pooled logistic",
  "Sensitivity: full hourly time-axis pooled logistic",
  "Sensitivity: weighted Cox model",
  "Sensitivity: pooled logistic with p01-p99 weight truncation",
  "Sensitivity: pooled logistic with p05-p95 weight truncation"
)
forest[, model := factor(model, levels = model_levels)]
forest[, contrast_label := factor(contrast_label, levels = c("3-6h vs 0-3h", "6-12h vs 0-3h", "6-12h vs 3-6h"))]
forest[, estimate_95_ci := fmt_est_ci(estimate, ci_low, ci_high)]
forest[, p_value_text := fmt_p(p_value)]

sens_table <- copy(forest)
sens_table[, `:=`(
  effect = paste0(measure, " ", estimate_95_ci),
  p_value = p_value_text
)]
sens_table <- sens_table[, .(
  model = as.character(model),
  contrast = as.character(contrast_label),
  effect,
  p_value
)]

fwrite(main_rows, out_main, bom = TRUE)
fwrite(sens_table, out_sens, bom = TRUE)
fwrite(forest, out_forest_data, bom = TRUE)

plot_status <- "not_attempted"
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  plot_dat <- copy(forest)
  plot_dat[, model_short := fcase(
    as.character(model) == "Main: coarse-time pooled logistic", "Main coarse-time",
    as.character(model) == "Sensitivity: full hourly time-axis pooled logistic", "Full hourly",
    as.character(model) == "Sensitivity: weighted Cox model", "Weighted Cox",
    as.character(model) == "Sensitivity: pooled logistic with p01-p99 weight truncation", "Trunc p01-p99",
    as.character(model) == "Sensitivity: pooled logistic with p05-p95 weight truncation", "Trunc p05-p95",
    default = as.character(model)
  )]
  plot_dat[, model_short := factor(model_short, levels = c(
    "Trunc p05-p95", "Trunc p01-p99", "Weighted Cox", "Full hourly", "Main coarse-time"
  ))]
  plot_dat[, label := paste0(measure, " ", estimate_95_ci, ", P=", p_value_text)]

  p <- ggplot(plot_dat, aes(x = estimate, y = model_short)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.16, linewidth = 0.5, color = "#3A3A3A") +
    geom_point(aes(shape = measure), size = 2.2, fill = "white", color = "#0B4F6C", stroke = 0.9) +
    facet_wrap(~ contrast_label, ncol = 1, scales = "free_y") +
    scale_x_log10(limits = c(0.85, 1.65), breaks = c(0.9, 1.0, 1.1, 1.25, 1.5)) +
    labs(
      x = "Effect estimate (log scale)",
      y = NULL,
      shape = NULL,
      title = "Antibiotic initiation window and 30-day mortality",
      subtitle = "Main model: coarse-time weighted pooled logistic; sensitivity models shown for robustness"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0),
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 9)
    )
  ggsave(out_forest_png, p, width = 7.2, height = 7.5, dpi = 300)
  plot_status <- "written"
} else {
  plot_status <- "ggplot2_not_available"
}

cat("=== formal_v1 main results tables and forest plot ===\n", file = out_report)
cat("main model: coarse-time weighted pooled logistic\n", file = out_report, append = TRUE)
cat("sensitivity models: full hourly pooled logistic; weighted Cox; p01-p99 and p05-p95 truncated weights\n\n", file = out_report, append = TRUE)
cat("main model table:\n", file = out_report, append = TRUE)
capture.output(print(main_rows), file = out_report, append = TRUE)
cat("\nsensitivity table:\n", file = out_report, append = TRUE)
capture.output(print(sens_table), file = out_report, append = TRUE)
cat("\noutputs:\n", file = out_report, append = TRUE)
cat(out_main, "\n", file = out_report, append = TRUE)
cat(out_sens, "\n", file = out_report, append = TRUE)
cat(out_forest_data, "\n", file = out_report, append = TRUE)
cat(out_forest_png, " plot_status=", plot_status, "\n", sep = "", file = out_report, append = TRUE)
cat("Done\n", file = out_report, append = TRUE)

cat("Wrote main results table:", out_main, "\n")
cat("Wrote sensitivity table:", out_sens, "\n")
cat("Wrote forest plot data:", out_forest_data, "\n")
cat("Forest plot status:", plot_status, "\n")
if (plot_status == "written") cat("Wrote forest plot:", out_forest_png, "\n")
cat("Wrote report:", out_report, "\n")
cat("PASS: results tables and forest plot generated.\n")
