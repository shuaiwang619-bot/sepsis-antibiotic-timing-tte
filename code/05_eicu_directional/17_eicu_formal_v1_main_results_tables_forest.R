# eICU formal_v1 step 17: main results tables and forest plot
# MIMIC counterpart: 17_formal_v1_main_results_tables_forest.R
#
# Scope:
#   Convert Step 10 pooled model outputs into review-ready result tables and
#   forest plots for the arm2 contrast:
#     3-12h vs 0-3h antibiotic start.
#
# Inputs:
#   outputs/models/eicu_formal_v1_arm2_model_exposure_summary.csv
#   outputs/models/eicu_formal_v1_arm2_model_fit_diagnostics.csv
#
# Outputs:
#   outputs/results/eicu_formal_v1_arm2_main_results_table.csv
#   outputs/results/eicu_formal_v1_arm2_main_results_table_readable.csv
#   outputs/results/eicu_formal_v1_arm2_forest_plot.png
#   outputs/results/eicu_formal_v1_arm2_forest_plot.pdf
#   outputs/results/eicu_formal_v1_combined_results_table_readable.csv
#   outputs/results/eicu_formal_v1_combined_forest_plot.png
#   outputs/results/eicu_formal_v1_arm2_results_report.txt

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

input_exposure <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "models", "eicu_formal_v1_arm2_model_exposure_summary.csv")
}
input_diag <- if (length(args) >= 3) {
  args[3]
} else {
  file.path(base_dir, "outputs", "models", "eicu_formal_v1_arm2_model_fit_diagnostics.csv")
}

out_dir <- file.path(base_dir, "outputs", "results")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_results <- file.path(out_dir, "eicu_formal_v1_arm2_main_results_table.csv")
out_results_readable <- file.path(out_dir, "eicu_formal_v1_arm2_main_results_table_readable.csv")
out_plot_png <- file.path(out_dir, "eicu_formal_v1_arm2_forest_plot.png")
out_plot_pdf <- file.path(out_dir, "eicu_formal_v1_arm2_forest_plot.pdf")
out_combined_results <- file.path(out_dir, "eicu_formal_v1_combined_results_table.csv")
out_combined_readable <- file.path(out_dir, "eicu_formal_v1_combined_results_table_readable.csv")
out_combined_plot_png <- file.path(out_dir, "eicu_formal_v1_combined_forest_plot.png")
out_combined_plot_pdf <- file.path(out_dir, "eicu_formal_v1_combined_forest_plot.pdf")
out_report <- file.path(out_dir, "eicu_formal_v1_arm2_results_report.txt")

if (!file.exists(input_exposure)) {
  stop("Exposure summary not found: ", input_exposure, call. = FALSE)
}
if (!file.exists(input_diag)) {
  stop("Fit diagnostics not found: ", input_diag, call. = FALSE)
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

fmt_num <- function(z, digits = 2) {
  ifelse(is.na(z), "", sprintf(paste0("%.", digits, "f"), z))
}

fmt_p <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  )
}

model_label_map <- c(
  crude_glm = "Crude logistic",
  main_glm = "Adjusted logistic",
  main_hospital_random_intercept_glmm = "Adjusted logistic + hospital random intercept",
  support_expanded_glm = "Support-expanded sensitivity",
  rare_comorbidity_glm = "Rare-comorbidity sensitivity"
)

model_order <- names(model_label_map)

x <- read.csv(input_exposure, stringsAsFactors = FALSE, check.names = FALSE)
d <- x[x$model_id %in% model_order, , drop = FALSE]
d$model_order <- match(d$model_id, model_order)
d <- d[order(d$model_order), , drop = FALSE]

numeric_cols <- c("or", "or_low", "or_high", "p_value", "estimate", "std_error", "conf_low", "conf_high")
for (cc in intersect(numeric_cols, names(d))) {
  d[[cc]] <- to_num(d[[cc]])
}

d$model_label <- unname(model_label_map[d$model_id])
d$contrast_label <- "3-12h vs 0-3h"
d$or_ci <- paste0(fmt_num(d$or, 2), " (", fmt_num(d$or_low, 2), "-", fmt_num(d$or_high, 2), ")")
d$p_value_fmt <- fmt_p(d$p_value)
d$direction <- ifelse(
  d$or > 1,
  "Late arm has higher estimated odds",
  "Late arm has lower estimated odds"
)
d$statistical_note <- ifelse(
  !is.na(d$p_value) & d$p_value < 0.05,
  "Nominally significant",
  "Not statistically significant"
)

results <- d[, c(
  "model_id",
  "model_label",
  "contrast_label",
  "or",
  "or_low",
  "or_high",
  "or_ci",
  "p_value",
  "p_value_fmt",
  "direction",
  "statistical_note",
  "m_used"
)]
write.csv(results, out_results, row.names = FALSE, na = "")

readable <- data.frame(
  Model = results$model_label,
  Contrast = results$contrast_label,
  `OR (95% CI)` = results$or_ci,
  `P value` = results$p_value_fmt,
  Interpretation = paste(results$direction, "-", results$statistical_note),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
write.csv(readable, out_results_readable, row.names = FALSE, na = "")

diag <- read.csv(input_diag, stringsAsFactors = FALSE, check.names = FALSE)
diag_summary <- do.call(rbind, lapply(split(diag, diag$model_id), function(z) {
  data.frame(
    model_id = z$model_id[1],
    fits_ok = sum(as.character(z$fit_ok) == "TRUE"),
    warnings_n = sum(nchar(as.character(z$warnings)) > 0),
    errors_n = sum(nchar(as.character(z$error)) > 0),
    singular_n = sum(as.character(z$singular) == "TRUE", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
diag_summary <- diag_summary[match(model_order, diag_summary$model_id), , drop = FALSE]
diag_summary <- diag_summary[!is.na(diag_summary$model_id), , drop = FALSE]

plot_df <- results
plot_df$model_label <- factor(plot_df$model_label, levels = rev(unname(model_label_map[model_order])))

draw_base_forest <- function(file, device = c("png", "pdf")) {
  device <- match.arg(device)
  if (device == "png") {
    grDevices::png(file, width = 1800, height = 1100, res = 220)
  } else {
    grDevices::pdf(file, width = 8.5, height = 5.2)
  }
  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(op)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mar = c(5, 13, 3, 3))
  y <- seq_len(nrow(plot_df))
  x_min <- min(plot_df$or_low, na.rm = TRUE) * 0.85
  x_max <- max(plot_df$or_high, na.rm = TRUE) * 1.15
  graphics::plot(
    plot_df$or,
    y,
    xlim = c(x_min, x_max),
    ylim = c(0.5, nrow(plot_df) + 0.5),
    yaxt = "n",
    log = "x",
    pch = 19,
    xlab = "Odds ratio for 30-day in-hospital death, 3-12h vs 0-3h",
    ylab = "",
    main = "eICU formal_v1 arm2 results"
  )
  graphics::axis(2, at = y, labels = as.character(plot_df$model_label), las = 1)
  graphics::abline(v = 1, lty = 2, col = "gray45")
  graphics::segments(plot_df$or_low, y, plot_df$or_high, y, lwd = 2)
  graphics::points(plot_df$or, y, pch = 19, cex = 1.1)
  graphics::text(
    x = x_max,
    y = y,
    labels = paste0(plot_df$or_ci, ", p=", plot_df$p_value_fmt),
    pos = 2,
    cex = 0.82
  )
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = or, y = model_label)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = or_low, xmax = or_high),
      orientation = "y",
      width = 0.18,
      linewidth = 0.7
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "eICU formal_v1 arm2 results",
      subtitle = "30-day in-hospital mortality; 3-12h vs 0-3h antibiotic start",
      x = "Odds ratio, log scale",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot"
    )
  ggplot2::ggsave(out_plot_png, p, width = 8.5, height = 4.8, dpi = 220)
  ggplot2::ggsave(out_plot_pdf, p, width = 8.5, height = 4.8)
} else {
  draw_base_forest(out_plot_png, "png")
  draw_base_forest(out_plot_pdf, "pdf")
}

sens_file <- file.path(base_dir, "outputs", "sensitivity", "eicu_formal_v1_12_72_exposure_summary.csv")
combined_outputs <- character()
combined_readable <- data.frame()
if (file.exists(sens_file)) {
  sens <- read.csv(sens_file, stringsAsFactors = FALSE, check.names = FALSE)
  sens_label_map <- c(
    sensitivity_crude_glm = "Sensitivity >12-72h: crude logistic",
    sensitivity_adjusted_glm = "Sensitivity >12-72h: adjusted logistic",
    sensitivity_hospital_random_intercept_glmm = "Sensitivity >12-72h: adjusted logistic + hospital random intercept",
    sensitivity_support_expanded_glm = "Sensitivity >12-72h: support-expanded",
    sensitivity_rare_comorbidity_glm = "Sensitivity >12-72h: rare-comorbidity"
  )
  sens_order <- names(sens_label_map)
  sens <- sens[sens$model_id %in% sens_order, , drop = FALSE]
  sens$model_order <- match(sens$model_id, sens_order)
  sens <- sens[order(sens$model_order), , drop = FALSE]
  for (cc in intersect(numeric_cols, names(sens))) {
    sens[[cc]] <- to_num(sens[[cc]])
  }
  sens$model_label <- unname(sens_label_map[sens$model_id])
  sens$contrast_label <- ">12-72h vs 0-3h"
  sens$or_ci <- paste0(fmt_num(sens$or, 2), " (", fmt_num(sens$or_low, 2), "-", fmt_num(sens$or_high, 2), ")")
  sens$p_value_fmt <- fmt_p(sens$p_value)
  sens$direction <- ifelse(
    sens$or > 1,
    "Late arm has higher estimated odds",
    "Late arm has lower estimated odds"
  )
  sens$statistical_note <- ifelse(
    !is.na(sens$p_value) & sens$p_value < 0.05,
    "Nominally significant",
    "Not statistically significant"
  )
  sens_results <- sens[, c(
    "model_id",
    "model_label",
    "contrast_label",
    "or",
    "or_low",
    "or_high",
    "or_ci",
    "p_value",
    "p_value_fmt",
    "direction",
    "statistical_note",
    "m_used"
  )]
  main_results <- results
  main_results$model_label <- paste0("Main arm2: ", main_results$model_label)
  combined <- rbind(main_results, sens_results)
  write.csv(combined, out_combined_results, row.names = FALSE, na = "")
  combined_readable <- data.frame(
    Model = combined$model_label,
    Contrast = combined$contrast_label,
    `OR (95% CI)` = combined$or_ci,
    `P value` = combined$p_value_fmt,
    Interpretation = paste(combined$direction, "-", combined$statistical_note),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  write.csv(combined_readable, out_combined_readable, row.names = FALSE, na = "")

  combined_plot <- combined
  combined_plot$model_label <- factor(combined_plot$model_label, levels = rev(combined_plot$model_label))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p2 <- ggplot2::ggplot(combined_plot, ggplot2::aes(x = or, y = model_label)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = or_low, xmax = or_high),
        orientation = "y",
        width = 0.18,
        linewidth = 0.65
      ) +
      ggplot2::geom_point(size = 2.1) +
      ggplot2::scale_x_log10() +
      ggplot2::labs(
        title = "eICU formal_v1 main and sensitivity results",
        subtitle = "30-day in-hospital mortality; reference = 0-3h antibiotic start",
        x = "Odds ratio, log scale",
        y = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title.position = "plot"
      )
    ggplot2::ggsave(out_combined_plot_png, p2, width = 10.5, height = 6.4, dpi = 220)
    ggplot2::ggsave(out_combined_plot_pdf, p2, width = 10.5, height = 6.4)
  }
  combined_outputs <- c(out_combined_results, out_combined_readable, out_combined_plot_png, out_combined_plot_pdf)
}

report_lines <- c(
  "eICU formal_v1 arm2 main results report",
  paste("exposure input:", input_exposure),
  paste("diagnostics input:", input_diag),
  "",
  "primary contrast:",
  "3-12h vs 0-3h antibiotic start; outcome = 30-day in-hospital death.",
  "",
  "result table:",
  capture.output(print(readable, row.names = FALSE)),
  "",
  "fit diagnostics summary:",
  capture.output(print(diag_summary, row.names = FALSE)),
  "",
  "interpretation:",
  "All point estimates are above 1, but all confidence intervals include 1.",
  "This supports a directionally higher estimated mortality odds in the 3-12h arm, without statistical significance in this eICU directional validation.",
  "",
  "outputs:",
  out_results,
  out_results_readable,
  out_plot_png,
  out_plot_pdf,
  if (length(combined_outputs) > 0) c("", "combined outputs:", combined_outputs) else character()
)
writeLines(report_lines, out_report)

message("Done. Results table: ", out_results_readable)
message("Forest plot: ", out_plot_png)
message("Report: ", out_report)
if (length(combined_outputs) > 0) {
  message("Combined outputs:")
  message(paste(combined_outputs, collapse = "\n"))
}
