# eICU formal_v1 step 11: arm2 CCW/IPCW covariate balance
# MIMIC counterpart: 11_formal_v1_ccw_covariate_balance.R
#
# Scope:
#   Report weighted standardized mean differences among adherent strategy
#   clones using final stabilized IPCW from Step 08.

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
weights_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_ipcw_final_weights_m5.csv")
}
weight_col <- if (length(args) >= 3) args[3] else "final_ipcw"

out_dir <- file.path(base_dir, "outputs", "ccw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_balance <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_balance_", weight_col, ".csv"))
out_summary <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_balance_", weight_col, "_summary.csv"))
out_report <- file.path(out_dir, paste0("eicu_formal_v1_ccw_arm2_balance_", weight_col, "_report.txt"))

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

wvar <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) < 2L) return(NA_real_)
  m <- sum(w[ok] * x[ok]) / sum(w[ok])
  sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])
}

smd_cont <- function(d, v, wcol) {
  d0 <- d[arm == "0-3h"]
  d1 <- d[arm == "3-12h"]
  x0 <- to_num(d0[[v]])
  x1 <- to_num(d1[[v]])
  w0 <- to_num(d0[[wcol]])
  w1 <- to_num(d1[[wcol]])
  m0 <- wmean(x0, w0)
  m1 <- wmean(x1, w1)
  v0 <- wvar(x0, w0)
  v1 <- wvar(x1, w1)
  denom <- sqrt((v0 + v1) / 2)
  smd <- if (is.finite(denom) && denom > 0) (m1 - m0) / denom else NA_real_
  data.table(
    variable = v,
    variable_type = "continuous",
    level = NA_character_,
    mean_0_3h = m0,
    mean_3_12h = m1,
    smd = smd,
    abs_smd = abs(smd)
  )
}

smd_cat <- function(d, v, wcol) {
  z <- as.character(d[[v]])
  z[is.na(z) | trimws(z) == ""] <- "Missing"
  lev <- sort(unique(z))
  rbindlist(lapply(lev, function(ll) {
    d0 <- d[arm == "0-3h"]
    d1 <- d[arm == "3-12h"]
    z0 <- as.numeric(as.character(d0[[v]]) == ll)
    z1 <- as.numeric(as.character(d1[[v]]) == ll)
    z0[is.na(z0)] <- 0
    z1[is.na(z1)] <- 0
    p0 <- wmean(z0, to_num(d0[[wcol]]))
    p1 <- wmean(z1, to_num(d1[[wcol]]))
    pbar <- (p0 + p1) / 2
    denom <- sqrt(pbar * (1 - pbar))
    smd <- if (is.finite(denom) && denom > 0) (p1 - p0) / denom else NA_real_
    data.table(
      variable = v,
      variable_type = "categorical",
      level = ll,
      mean_0_3h = p0,
      mean_3_12h = p1,
      smd = smd,
      abs_smd = abs(smd)
    )
  }), fill = TRUE)
}

if (!file.exists(weights_file)) stop("Weights file not found: ", weights_file, call. = FALSE)

d <- fread(weights_file, showProgress = FALSE)
if (!weight_col %in% names(d)) stop("Weight column not found: ", weight_col, call. = FALSE)

d <- d[strategy_adherent_by_delay == TRUE]
d[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  arm = as.character(arm),
  use_weight = to_num(get(weight_col))
)]
d <- d[arm %in% c("0-3h", "3-12h") & is.finite(use_weight) & use_weight > 0]

if (d[, anyDuplicated(paste(.imp, patientunitstayid))] > 0) {
  stop("Adherent clone balance set has duplicate .imp + patientunitstayid rows.", call. = FALSE)
}

continuous_vars <- c("age_model", "apache_score_iva_model")
categorical_vars <- c(
  "gender_factor", "ethnicity_group", "unit_admit_source_group", "unit_type_group",
  "hosp_window_volume_bucket_factor", "hosp_zero_abx_reporting_ind",
  "diabetes_model", "vent_model", "intubated_model", "dialysis_model",
  "cirrhosis_model", "immunosuppression_model", "hepaticfailure_model", "metastaticcancer_model",
  "apache_score_iva_model_missing_ind", "diabetes_model_missing_ind"
)

continuous_vars <- intersect(continuous_vars, names(d))
categorical_vars <- intersect(categorical_vars, names(d))

balance <- rbindlist(lapply(sort(unique(d$.imp)), function(ii) {
  dd <- d[.imp == ii]
  out <- rbindlist(c(
    lapply(continuous_vars, function(v) smd_cont(dd, v, "use_weight")),
    lapply(categorical_vars, function(v) smd_cat(dd, v, "use_weight"))
  ), fill = TRUE)
  out[, .imp := ii]
  out
}), fill = TRUE)

setcolorder(balance, c(".imp", "variable", "variable_type", "level"))
setorder(balance, .imp, -abs_smd, variable, level)

summary <- balance[, .(
  levels_or_rows = .N,
  mean_abs_smd = mean(abs_smd, na.rm = TRUE),
  max_abs_smd = max(abs_smd, na.rm = TRUE)
), by = variable][order(-max_abs_smd)]

summary_overall <- data.table(
  variable = "ALL",
  levels_or_rows = nrow(balance),
  mean_abs_smd = mean(balance$abs_smd, na.rm = TRUE),
  max_abs_smd = max(balance$abs_smd, na.rm = TRUE)
)
summary <- rbind(summary_overall, summary, fill = TRUE)

fwrite(balance, out_balance)
fwrite(summary, out_summary)

report_lines <- c(
  "eICU formal_v1 step 11 arm2 CCW/IPCW covariate balance",
  paste("weights_file:", weights_file),
  paste("weight_col:", weight_col),
  "",
  "balance summary:",
  capture.output(print(summary)),
  "",
  "notes:",
  "Balance is calculated among adherent clones only.",
  "For categorical variables, the summary reports the maximum absolute SMD across levels.",
  "Weights are final stabilized IPCW from Step 08 unless another weight column is passed."
)
writeLines(report_lines, out_report)

message("Wrote balance: ", out_balance)
message("Wrote balance summary: ", out_summary)
message("Wrote report: ", out_report)
