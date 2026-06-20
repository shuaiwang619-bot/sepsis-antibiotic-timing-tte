# formal_v1 long-table pre-CCW QC
# Scope: read-only checks before clone-censor-weighting.
# This script does not write intermediate data files.

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

long_file <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "formal_v1_cov_tvcov_ccw_model_input.csv")
}

wide_imp_file <- if (length(args) >= 3) {
  args[3]
} else {
  file.path(base_dir, "formal_v1_analysis_main_mice_m5_model_ready_long.csv")
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

library(data.table)

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) stop(label, " not found: ", path, call. = FALSE)
}

fmt_pct <- function(x) sprintf("%.2f%%", 100 * x)

miss_n <- function(x) sum(is.na(x) | trimws(as.character(x)) == "")

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

summarise_numeric <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(data.table())
  rbindlist(lapply(cols, function(v) {
    x <- suppressWarnings(as.numeric(dat[[v]]))
    data.table(
      variable = v,
      missing_n = sum(is.na(x)),
      missing_pct = fmt_pct(mean(is.na(x))),
      min = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      p25 = if (all(is.na(x))) NA_real_ else as.numeric(quantile(x, 0.25, na.rm = TRUE, names = FALSE)),
      median = if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE),
      p75 = if (all(is.na(x))) NA_real_ else as.numeric(quantile(x, 0.75, na.rm = TRUE, names = FALSE)),
      max = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    )
  }))
}

summarise_levels <- function(dat, cols, max_levels = 12L) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(data.table())
  rbindlist(lapply(cols, function(v) {
    tab <- dat[, .N, by = v][order(-N)]
    setnames(tab, v, "level")
    tab[, variable := v]
    tab[, pct := fmt_pct(N / sum(N))]
    tab[seq_len(min(.N, max_levels)), .(variable, level, N, pct)]
  }))
}

stop_if_missing(long_file, "Long table")
stop_if_missing(wide_imp_file, "Imputed wide table")

cat("formal_v1 long-table pre-CCW QC\n")
cat("Run time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Long table:", long_file, "\n")
cat("Imputed wide table:", wide_imp_file, "\n")

long <- fread(long_file, showProgress = FALSE)
wide <- fread(wide_imp_file, showProgress = FALSE)

required_long <- c(
  "stay_id", "subject_id", "hadm_id", "sepsis_t0", "t_hour", "grid_time",
  "time_from_t0_h", "still_observed", "sofa_total_tvcov",
  "meanbp_min_tvcov", "shock_tvcov_b", "lactate_tvcov",
  "abx_started_by_hour", "at_risk_for_initiation", "first_abx_delay_hours",
  "sofa_tvcov_missing", "shock_tvcov_missing", "lactate_tvcov_missing"
)

required_wide <- c(
  ".imp", "stay_id", "subject_id", "hadm_id", "abx_window_factor",
  "ccw_role_factor", "death_30d", "fu_time_30d_days",
  "age_at_icu_model", "sex_factor", "race_group",
  "admission_type_group", "charlson_comorbidity_index_model",
  "sofa_total_t0_imp", "sofa_total_t0_missing", "shock_t0_b_factor",
  "meanbp_min_t0_imp_sens", "meanbp_min_t0_missing"
)

print_section("1. Required Columns")
cat("Long missing columns:", paste(setdiff(required_long, names(long)), collapse = ", "), "\n")
cat("Wide missing columns:", paste(setdiff(required_wide, names(wide)), collapse = ", "), "\n")

if (length(setdiff(c("stay_id", "t_hour"), names(long))) > 0) {
  stop("Long table must contain stay_id and t_hour before QC can continue.", call. = FALSE)
}
if (length(setdiff(c(".imp", "stay_id"), names(wide))) > 0) {
  stop("Wide imputed table must contain .imp and stay_id before QC can continue.", call. = FALSE)
}

print_section("2. Shape And IDs")
shape <- data.table(
  table = c("long", "wide_imp"),
  rows = c(nrow(long), nrow(wide)),
  cols = c(ncol(long), ncol(wide)),
  unique_stay_id = c(uniqueN(long$stay_id), uniqueN(wide$stay_id))
)
print(shape)

imp_counts <- wide[, .(rows = .N, unique_stay_id = uniqueN(stay_id)), by = .imp][order(.imp)]
print(imp_counts)

wide_key_dups <- wide[, .N, by = .(.imp, stay_id)][N > 1]
long_key_dups <- long[, .N, by = .(stay_id, t_hour)][N > 1]
cat("Wide duplicate .imp + stay_id rows:", nrow(wide_key_dups), "\n")
cat("Long duplicate stay_id + t_hour rows:", nrow(long_key_dups), "\n")

print_section("3. Time Grid")
time_summary <- long[, .(
  rows = .N,
  unique_stay_id = uniqueN(stay_id),
  min_t_hour = min(t_hour, na.rm = TRUE),
  max_t_hour = max(t_hour, na.rm = TRUE),
  min_time_from_t0_h = min(time_from_t0_h, na.rm = TRUE),
  max_time_from_t0_h = max(time_from_t0_h, na.rm = TRUE),
  median_rows_per_stay = median(.N),
  min_rows_per_stay = min(.N),
  max_rows_per_stay = max(.N)
), by = stay_id][, .(
  unique_stay_id = .N,
  min_t_hour = min(min_t_hour, na.rm = TRUE),
  max_t_hour = max(max_t_hour, na.rm = TRUE),
  min_time_from_t0_h = min(min_time_from_t0_h, na.rm = TRUE),
  max_time_from_t0_h = max(max_time_from_t0_h, na.rm = TRUE),
  median_rows_per_stay = median(rows),
  min_rows_per_stay = min(rows),
  max_rows_per_stay = max(rows)
)]
print(time_summary)

t_hour_counts <- long[, .(rows = .N, unique_stay_id = uniqueN(stay_id)), by = t_hour][order(t_hour)]
cat("First 8 t_hour rows:\n")
print(head(t_hour_counts, 8))
cat("Last 8 t_hour rows:\n")
print(tail(t_hour_counts, 8))

print_section("4. Observation And Initiation Fields")
level_cols <- intersect(
  c("still_observed", "abx_started_by_hour", "at_risk_for_initiation",
    "shock_tvcov_b", "sofa_tvcov_missing", "shock_tvcov_missing",
    "lactate_tvcov_missing"),
  names(long)
)
print(summarise_levels(long, level_cols))

if ("first_abx_delay_hours" %in% names(long)) {
  one_delay <- unique(long[, .(stay_id, first_abx_delay_hours)])
  delay_dups <- one_delay[, .N, by = stay_id][N > 1]
  cat("Stays with multiple first_abx_delay_hours values:", nrow(delay_dups), "\n")
  print(summarise_numeric(one_delay, "first_abx_delay_hours"))
}

print_section("5. Time-Varying Covariates")
print(summarise_numeric(
  long,
  c("sofa_total_tvcov", "meanbp_min_tvcov", "gcs_min_tvcov",
    "uo_24hr_tvcov", "lactate_tvcov")
))

print_section("6. Wide-To-Long Merge Readiness")
wide0 <- wide[.imp == 0]
wide15 <- wide[.imp %in% 1:5]
cat(".imp=0 rows:", nrow(wide0), " unique stays:", uniqueN(wide0$stay_id), "\n")
cat(".imp=1-5 rows:", nrow(wide15), " unique stays:", uniqueN(wide15$stay_id), "\n")

long_ids <- unique(long$stay_id)
wide_ids <- unique(wide0$stay_id)
cat("Long stays missing from wide .imp=0:", length(setdiff(long_ids, wide_ids)), "\n")
cat("Wide .imp=0 stays missing from long:", length(setdiff(wide_ids, long_ids)), "\n")

role_tab <- wide0[, .N, by = .(ccw_role_factor, abx_window_factor)][order(ccw_role_factor, abx_window_factor)]
print(role_tab)

analysis_ids <- wide0[ccw_role_factor == "analyzable", unique(stay_id)]
cat("Analyzable stays in wide .imp=0:", length(analysis_ids), "\n")
cat("Analyzable stays present in long:", length(intersect(analysis_ids, long_ids)), "\n")

print_section("7. New Boundary Cross-Check")
if ("first_abx_delay_hours" %in% names(long) && "abx_window_factor" %in% names(wide0)) {
  delay_by_stay <- unique(long[, .(stay_id, first_abx_delay_hours)])
  delay_by_stay <- delay_by_stay[, .SD[1], by = stay_id]
  chk <- merge(
    wide0[, .(stay_id, ccw_role_factor, abx_window_factor)],
    delay_by_stay,
    by = "stay_id",
    all.x = TRUE
  )
  chk[, delay_window_medical := fifelse(
    is.na(first_abx_delay_hours), NA_character_,
    fifelse(first_abx_delay_hours >= 0 & first_abx_delay_hours <= 3, "0-3h",
      fifelse(first_abx_delay_hours > 3 & first_abx_delay_hours <= 6, "3-6h",
        fifelse(first_abx_delay_hours > 6 & first_abx_delay_hours <= 12, "6-12h",
          fifelse(first_abx_delay_hours > 12 & first_abx_delay_hours <= 72, ">=12h", "outside_0_72h")
        )
      )
    )
  )]
  print(chk[, .N, by = .(ccw_role_factor, abx_window_factor, delay_window_medical)][order(ccw_role_factor, abx_window_factor, delay_window_medical)])
  mismatch <- chk[
    ccw_role_factor == "analyzable" &
      !is.na(delay_window_medical) &
      abx_window_factor %in% c("0-3h", "3-6h", "6-12h", ">=12h") &
      abx_window_factor != delay_window_medical
  ]
  cat("Analyzable boundary mismatches:", nrow(mismatch), "\n")
}

print_section("8. QC Verdict")
blocking <- c(
  length(setdiff(required_long, names(long))) > 0,
  length(setdiff(required_wide, names(wide))) > 0,
  nrow(wide_key_dups) > 0,
  nrow(long_key_dups) > 0,
  length(setdiff(long_ids, wide_ids)) > 0
)

if (any(blocking)) {
  cat("VERDICT: BLOCKED - fix required-column/key/ID issues before CCW.\n")
} else {
  cat("VERDICT: PASS - long table is merge-ready for imputed wide baseline covariates.\n")
  cat("NEXT: build 5 imputed long datasets, clone 0-3h/3-6h/6-12h arms, then apply artificial censoring.\n")
}
