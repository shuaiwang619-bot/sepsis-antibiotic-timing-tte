# formal_v1 CCW clone and artificial-censoring diagnostics
# Scope: use the MICE-aware long table to check three-arm cloning and censoring.
# Default mode is diagnostic only and does not write large intermediate tables.

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
write_large_output <- if (length(args) >= 2) {
  tolower(args[2]) %in% c("true", "t", "1", "yes", "y", "write")
} else {
  FALSE
}

long_ready_file <- file.path(base_dir, "formal_v1_cov_tvcov_ccw_model_ready_m5.csv")
wide_imp_file <- file.path(base_dir, "formal_v1_analysis_main_mice_m5_model_ready_long.csv")
out_file <- file.path(base_dir, "formal_v1_ccw_clone_censor_m5_long.csv")

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

medical_delay_window <- function(delay) {
  out <- rep(NA_character_, length(delay))
  out[!is.na(delay) & delay >= 0 & delay <= 3] <- "0-3h"
  out[!is.na(delay) & delay > 3 & delay <= 6] <- "3-6h"
  out[!is.na(delay) & delay > 6 & delay <= 12] <- "6-12h"
  out[!is.na(delay) & delay > 12 & delay <= 72] <- ">=12h"
  out
}

censor_hour <- function(arm, delay) {
  out <- rep(NA_real_, length(delay))

  i <- arm == "0-3h" & (is.na(delay) | delay > 3)
  out[i] <- 3

  i <- arm == "3-6h" & !is.na(delay) & delay <= 3
  out[i] <- pmax(0, ceiling(delay[i]))
  i <- arm == "3-6h" & (is.na(delay) | delay > 6)
  out[i] <- 6

  i <- arm == "6-12h" & !is.na(delay) & delay <= 6
  out[i] <- pmax(0, ceiling(delay[i]))
  i <- arm == "6-12h" & (is.na(delay) | delay > 12)
  out[i] <- 12

  out
}

censor_reason <- function(arm, delay) {
  out <- rep("adherent", length(delay))
  out[arm == "0-3h" & (is.na(delay) | delay > 3)] <- "late_or_no_init"
  out[arm == "3-6h" & !is.na(delay) & delay <= 3] <- "early_init"
  out[arm == "3-6h" & (is.na(delay) | delay > 6)] <- "late_or_no_init"
  out[arm == "6-12h" & !is.na(delay) & delay <= 6] <- "early_init"
  out[arm == "6-12h" & (is.na(delay) | delay > 12)] <- "late_or_no_init"
  out
}

stop_if_missing(long_ready_file, "MICE-aware long table")
stop_if_missing(wide_imp_file, "Imputed wide table")

cat("formal_v1 CCW clone-censor diagnostics\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("MICE-aware long table:", long_ready_file, "\n")
cat("Imputed wide table:", wide_imp_file, "\n")
cat("Write large cloned table:", write_large_output, "\n")

need_long <- c(
  ".imp", "stay_id", "subject_id", "hadm_id", "t_hour", "t_hour_clean",
  "still_observed_i", "first_abx_delay_hours_clean",
  "sofa_total_tvcov_fill", "shock_tvcov_b_fill",
  "sofa_total_tvcov_missing_model", "shock_tvcov_b_missing_model"
)
need_wide <- c(
  ".imp", "stay_id", "subject_id", "hadm_id", "ccw_role_factor",
  "abx_window_factor", "death_30d", "fu_time_30d_days"
)

long_header <- names(fread(long_ready_file, nrows = 0, showProgress = FALSE))
wide_header <- names(fread(wide_imp_file, nrows = 0, showProgress = FALSE))
missing_long <- setdiff(need_long, long_header)
missing_wide <- setdiff(need_wide, wide_header)
if (length(missing_long) > 0) stop("Long missing columns: ", paste(missing_long, collapse = ", "), call. = FALSE)
if (length(missing_wide) > 0) stop("Wide missing columns: ", paste(missing_wide, collapse = ", "), call. = FALSE)

long <- fread(long_ready_file, select = need_long, showProgress = FALSE)
wide <- fread(wide_imp_file, select = need_wide, showProgress = FALSE)

long[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  t_hour_clean = as_num(t_hour_clean),
  still_observed_i = as.integer(still_observed_i),
  first_abx_delay_hours_clean = as_num(first_abx_delay_hours_clean)
)]
wide[, `:=`(
  .imp = as.integer(.imp),
  stay_id = as.integer(stay_id),
  death_30d = as.integer(death_30d),
  fu_time_30d_days = as_num(fu_time_30d_days)
)]

wide <- wide[.imp %in% 1:5]
long <- long[.imp %in% 1:5]

if (wide[, anyDuplicated(paste(.imp, stay_id))] > 0) {
  stop("Duplicate .imp + stay_id rows in imputed wide data.", call. = FALSE)
}
if (long[, anyDuplicated(paste(.imp, stay_id, t_hour_clean))] > 0) {
  stop("Duplicate .imp + stay_id + t_hour_clean rows in MICE-aware long data.", call. = FALSE)
}

long_shape <- long[, .(
  long_rows = .N,
  stays = uniqueN(stay_id),
  min_t = min(t_hour_clean, na.rm = TRUE),
  max_t = max(t_hour_clean, na.rm = TRUE)
), by = .imp][order(.imp)]

rows_per_stay <- long[, .N, by = .(.imp, stay_id)]
bad_rows_per_stay <- rows_per_stay[N != 25L]
if (nrow(bad_rows_per_stay) > 0) {
  stop("Some .imp + stay_id do not have 25 long rows.", call. = FALSE)
}

delay_by_stay <- long[, .(
  first_abx_delay_hours_clean = first(first_abx_delay_hours_clean),
  delay_n_values = uniqueN(first_abx_delay_hours_clean)
), by = .(.imp, stay_id)]
if (delay_by_stay[delay_n_values > 1L, .N] > 0) {
  stop("first_abx_delay_hours_clean is not stable within .imp + stay_id.", call. = FALSE)
}
delay_by_stay[, delay_n_values := NULL]
delay_by_stay[, delay_window_medical := medical_delay_window(first_abx_delay_hours_clean)]

eligible_wide <- wide[ccw_role_factor %in% c("analyzable", "censor_noinit")]
eligible_wide <- merge(
  eligible_wide,
  delay_by_stay,
  by = c(".imp", "stay_id"),
  all.x = TRUE,
  sort = FALSE
)

if (eligible_wide[, any(is.na(subject_id) | is.na(hadm_id))]) {
  stop("Eligible wide metadata has missing subject_id or hadm_id.", call. = FALSE)
}

boundary_check <- eligible_wide[ccw_role_factor == "analyzable", .(
  analyzable = .N,
  mismatch = sum(abx_window_factor != delay_window_medical, na.rm = TRUE),
  missing_delay_window = sum(is.na(delay_window_medical))
), by = .imp][order(.imp)]

arm_def <- data.table(
  arm = c("0-3h", "3-6h", "6-12h"),
  arm_id = 1:3,
  arm_start_h = c(0, 3, 6),
  arm_end_h = c(3, 6, 12)
)

clones <- eligible_wide[, .(
  .imp,
  stay_id,
  subject_id,
  hadm_id,
  ccw_role_factor,
  observed_abx_window = abx_window_factor,
  delay_window_medical,
  first_abx_delay_hours_clean,
  death_30d,
  fu_time_30d_days
)]

clones[, tmp_cross_join := 1L]
arm_def[, tmp_cross_join := 1L]
clones <- merge(clones, arm_def, by = "tmp_cross_join", allow.cartesian = TRUE, sort = FALSE)
clones[, tmp_cross_join := NULL]
arm_def[, tmp_cross_join := NULL]

clones[, artificial_censor_h := censor_hour(arm, first_abx_delay_hours_clean)]
clones[, artificial_censor_reason := censor_reason(arm, first_abx_delay_hours_clean)]
clones[, strategy_adherent_by_delay := artificial_censor_reason == "adherent"]
clones[, clone_id := paste(.imp, stay_id, arm, sep = "_")]

print_section("1. Long Table Shape")
print(long_shape)
cat("Rows per .imp + stay_id:", rows_per_stay[, unique(N)], "\n")

print_section("2. Clone Shape")
cat("Eligible stays per imputation:", eligible_wide[, uniqueN(stay_id)], "\n")
cat("Eligible source rows:", nrow(eligible_wide), "\n")
cat("Stay-arm clones:", nrow(clones), "\n")
cat("Expected full long rows if expanded:", nrow(clones) * rows_per_stay[, unique(N)], "\n")

print_section("3. Eligible Roles")
print(eligible_wide[, .(stays = uniqueN(stay_id), rows = .N), by = .(.imp, ccw_role_factor)][order(.imp, ccw_role_factor)])

print_section("4. Boundary Check")
print(boundary_check)

print_section("5. Artificial Censoring By Arm")
print(clones[, .(
  clones = .N,
  observed_analyzable = sum(ccw_role_factor == "analyzable"),
  observed_noinit = sum(ccw_role_factor == "censor_noinit"),
  adherent = sum(strategy_adherent_by_delay),
  early_init = sum(artificial_censor_reason == "early_init"),
  late_or_no_init = sum(artificial_censor_reason == "late_or_no_init")
), by = .(.imp, arm)][order(.imp, arm)])

print_section("6. Censor Event Hour Distribution")
print(clones[, .N, by = .(.imp, arm, artificial_censor_reason, artificial_censor_h)][
  order(.imp, arm, artificial_censor_reason, artificial_censor_h)
])

print_section("7. Observed Window Cross-Tab")
print(clones[, .N, by = .(.imp, arm, ccw_role_factor, observed_abx_window, delay_window_medical, artificial_censor_reason)][
  order(.imp, arm, ccw_role_factor, observed_abx_window, delay_window_medical, artificial_censor_reason)
])

print_section("8. Estimated Kept Rows")
max_t <- long_shape[, max(max_t)]
clones[, kept_rows_through_censor_event := fifelse(
  is.na(artificial_censor_h),
  max_t + 1,
  pmax(0, pmin(max_t, artificial_censor_h) + 1)
)]
clones[, kept_rows_before_censor_event := fifelse(
  is.na(artificial_censor_h),
  max_t + 1,
  pmax(0, pmin(max_t + 1, artificial_censor_h))
)]
print(clones[, .(
  full_rows = .N * (max_t + 1),
  rows_through_censor_event = sum(kept_rows_through_censor_event),
  rows_before_censor_event = sum(kept_rows_before_censor_event)
), by = .(.imp, arm)][order(.imp, arm)])

print_section("9. Main IPCW Matrix Check")
main_ipcw_na <- long[still_observed_i == 1L & t_hour_clean <= 12, .(
  rows = .N,
  sofa_fill_na = sum(is.na(sofa_total_tvcov_fill)),
  shock_fill_na = sum(is.na(shock_tvcov_b_fill)),
  sofa_missing_flag_na = sum(is.na(sofa_total_tvcov_missing_model)),
  shock_missing_flag_na = sum(is.na(shock_tvcov_b_missing_model))
), by = .imp][order(.imp)]
print(main_ipcw_na)

if (main_ipcw_na[, any(sofa_fill_na > 0 | shock_fill_na > 0 | sofa_missing_flag_na > 0 | shock_missing_flag_na > 0)]) {
  stop("Main IPCW design variables still contain NA.", call. = FALSE)
}

if (write_large_output) {
  print_section("10. Writing Large Clone-Censor Long Table")
  keep_long_cols <- c(
    ".imp", "stay_id", "t_hour", "t_hour_clean", "grid_time", "time_from_t0_h",
    "still_observed_i", "sofa_total_tvcov_fill", "shock_tvcov_b_fill",
    "sofa_total_tvcov_missing_model", "shock_tvcov_b_missing_model",
    "abx_started_by_hour_i", "at_risk_for_initiation_i"
  )
  keep_long_cols <- intersect(keep_long_cols, long_header)
  long_small <- fread(long_ready_file, select = keep_long_cols, showProgress = FALSE)
  long_small[, `:=`(.imp = as.integer(.imp), stay_id = as.integer(stay_id), t_hour_clean = as_num(t_hour_clean))]
  out <- long_small[clones, on = c(".imp", "stay_id"), allow.cartesian = TRUE, nomatch = 0]
  out[, artificial_censor_event := as.integer(!is.na(artificial_censor_h) & t_hour_clean == artificial_censor_h)]
  out[, post_artificial_censor := as.integer(!is.na(artificial_censor_h) & t_hour_clean > artificial_censor_h)]
  out[, uncensored_through_hour := as.integer(still_observed_i == 1L & post_artificial_censor == 0L)]
  setcolorder(out, c(".imp", "clone_id", "arm", "arm_id", "stay_id", "t_hour_clean"))
  fwrite(out, out_file)
  cat("Wrote:", out_file, "\n")
  cat("Rows:", nrow(out), "\n")
}

print_section(if (write_large_output) "11. Verdict" else "10. Verdict")
cat("PASS: clone and artificial-censoring rules are constructible for .imp=1-5.\n")
cat("PASS: main IPCW design columns have no NA among observed rows through 12h.\n")
if (!write_large_output) {
  cat("Default run did not write a large clone-censor table.\n")
}
