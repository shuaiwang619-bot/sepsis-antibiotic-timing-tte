# eICU formal_v1 step 07: arm2 CCW clone and censor diagnostics
# MIMIC counterpart: 07_formal_v1_ccw_clone_censor_diagnostics.R
#
# Scope:
#   Clone each completed-imputation arm2 patient into two strategies:
#     0-3h vs 3-12h antibiotic initiation from ICU admission.
#   Apply artificial censoring rules and write compact clone diagnostics.

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
hourly_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "ccw", "eicu_formal_v1_ccw_arm2_hourly_input_m5.csv")
}

out_dir <- file.path(base_dir, "outputs", "ccw")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_clones <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_clones_m5.csv")
out_diag <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_clone_censor_diagnostics.csv")
out_report <- file.path(out_dir, "eicu_formal_v1_ccw_arm2_clone_censor_report.txt")

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}
library(data.table)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

censor_hour <- function(arm, delay) {
  out <- rep(NA_real_, length(delay))

  i <- arm == "0-3h" & (is.na(delay) | delay > 3)
  out[i] <- 3

  i <- arm == "3-12h" & !is.na(delay) & delay <= 3
  out[i] <- pmax(0, ceiling(delay[i]))
  i <- arm == "3-12h" & (is.na(delay) | delay > 12)
  out[i] <- 12

  out
}

censor_reason <- function(arm, delay) {
  out <- rep("adherent", length(delay))
  out[arm == "0-3h" & (is.na(delay) | delay > 3)] <- "late_or_no_init"
  out[arm == "3-12h" & !is.na(delay) & delay <= 3] <- "early_init"
  out[arm == "3-12h" & (is.na(delay) | delay > 12)] <- "late_or_no_init"
  out
}

stop_if_missing(hourly_file, "Step 06 hourly input")

hourly <- fread(hourly_file, showProgress = FALSE)
need <- c(
  ".imp", "patientunitstayid", "uniquepid", "hospitalid",
  "death_30d_inhosp", "arm2_abx_timing", "arm2_late_3_12h",
  "first_abx_delay_hours_from_unit", "followup_30d_inhosp_days",
  "surv_hours_30d"
)
missing_need <- setdiff(need, names(hourly))
if (length(missing_need) > 0) stop("Hourly input missing columns: ", paste(missing_need, collapse = ", "), call. = FALSE)

hourly[, `:=`(
  .imp = as.integer(to_num(.imp)),
  patientunitstayid = as.integer(to_num(patientunitstayid)),
  t_hour = as.integer(to_num(t_hour)),
  first_abx_delay_hours_from_unit = to_num(first_abx_delay_hours_from_unit),
  death_30d_inhosp = as.integer(to_num(death_30d_inhosp)),
  followup_30d_inhosp_days = to_num(followup_30d_inhosp_days),
  surv_hours_30d = to_num(surv_hours_30d)
)]

if (hourly[, anyDuplicated(paste(.imp, patientunitstayid, t_hour))] > 0) {
  stop("Duplicate .imp + patientunitstayid + t_hour rows in hourly input.", call. = FALSE)
}

base <- hourly[t_hour == 0]
if (base[, anyDuplicated(paste(.imp, patientunitstayid))] > 0) {
  stop("Duplicate .imp + patientunitstayid rows at t_hour 0.", call. = FALSE)
}

arm_def <- data.table(
  arm = c("0-3h", "3-12h"),
  arm_id = 1:2,
  arm_start_h = c(0L, 3L),
  arm_end_h = c(3L, 12L)
)

base[, tmp_cross_join := 1L]
arm_def[, tmp_cross_join := 1L]
clones <- merge(base, arm_def, by = "tmp_cross_join", allow.cartesian = TRUE, sort = FALSE)
clones[, tmp_cross_join := NULL]
base[, tmp_cross_join := NULL]
arm_def[, tmp_cross_join := NULL]

clones[, artificial_censor_h := censor_hour(arm, first_abx_delay_hours_from_unit)]
clones[, artificial_censor_reason := censor_reason(arm, first_abx_delay_hours_from_unit)]
clones[, strategy_adherent_by_delay := artificial_censor_reason == "adherent"]
clones[, clone_id := paste(.imp, patientunitstayid, arm, sep = "_")]
clones[, natural_censor_before_arm_end := as.integer(
  strategy_adherent_by_delay & surv_hours_30d < arm_end_h
)]

diag_shape <- clones[, .(
  clones = .N,
  stays = uniqueN(patientunitstayid),
  adherent = sum(strategy_adherent_by_delay),
  artificial_censored = sum(!strategy_adherent_by_delay),
  natural_censor_before_arm_end = sum(natural_censor_before_arm_end)
), by = .(.imp, arm, arm_id)][order(.imp, arm_id)]

diag_reason <- clones[, .N, by = .(
  .imp, arm, arm_id, artificial_censor_reason, artificial_censor_h
)][order(.imp, arm_id, artificial_censor_reason, artificial_censor_h)]

diag_observed <- clones[, .(
  clones = .N,
  adherent = sum(strategy_adherent_by_delay),
  artificial_censored = sum(!strategy_adherent_by_delay)
), by = .(.imp, arm, arm_id, observed_arm = arm2_abx_timing)][order(.imp, arm_id, observed_arm)]

diag_out <- rbindlist(list(
  cbind(section = "shape", diag_shape),
  cbind(section = "reason", diag_reason),
  cbind(section = "observed_by_assigned", diag_observed)
), fill = TRUE)

setorder(clones, .imp, patientunitstayid, arm_id)
fwrite(clones, out_clones)
fwrite(diag_out, out_diag)

report_lines <- c(
  "eICU formal_v1 step 07 arm2 clone/censor diagnostics",
  paste("hourly_file:", hourly_file),
  "",
  "strategy definitions:",
  "0-3h: antibiotic initiation from ICU admission through 3h; censor at 3h if later/no initiation.",
  "3-12h: no antibiotic before >3h and initiation through 12h; censor at observed early initiation if <=3h, or at 12h if later/no initiation.",
  "",
  "clone shape:",
  capture.output(print(diag_shape)),
  "",
  "censor reasons:",
  capture.output(print(diag_reason)),
  "",
  "observed arm by assigned arm:",
  capture.output(print(diag_observed))
)
writeLines(report_lines, out_report)

message("Wrote clones: ", out_clones)
message("Wrote diagnostics: ", out_diag)
message("Wrote report: ", out_report)
