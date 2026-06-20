# eICU formal_v1 step 03: analysis_main categorical encoding
# MIMIC counterpart: 03_formal_v1_analysis_main_categorical_encoding.R
#
# Scope:
#   Categorical variables only. No numeric outlier handling and no imputation.
#
# Input:
#   outputs/wide_tables/eicu_formal_v1_analysis_main_model_ready_wide.csv
#   or "db" to read public.eicu_formal_v1_analysis_main_model_ready_wide.
#
# Output:
#   outputs/wide_tables/eicu_formal_v1_analysis_main_categorical_encoded.csv

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
input_source <- if (length(args) >= 2) {
  args[2]
} else {
  file.path(base_dir, "outputs", "wide_tables", "eicu_formal_v1_analysis_main_model_ready_wide.csv")
}

out_dir <- file.path(base_dir, "outputs", "wide_tables")
qc_dir <- file.path(base_dir, "outputs", "qc")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

out_encoded <- file.path(out_dir, "eicu_formal_v1_analysis_main_categorical_encoded.csv")
out_counts <- file.path(qc_dir, "eicu_formal_v1_step03_categorical_counts.csv")
out_roles <- file.path(qc_dir, "eicu_formal_v1_step03_categorical_variable_roles.csv")
out_report <- file.path(qc_dir, "eicu_formal_v1_step03_categorical_encoding_report.txt")

is_miss <- function(z) {
  zz <- trimws(as.character(z))
  is.na(z) | zz == "" | toupper(zz) %in% c("NA", "NULL", "NAN")
}

clean_text <- function(z) {
  out <- trimws(as.character(z))
  out[is_miss(z)] <- NA_character_
  out
}

to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))

factor_with_missing <- function(z, missing_level = "Missing") {
  out <- clean_text(z)
  out[is.na(out)] <- missing_level
  droplevels(factor(out))
}

binary_factor <- function(z, yes = "Yes", no = "No", missing = "Missing") {
  zz <- to_num(z)
  out <- ifelse(is.na(zz), missing, ifelse(zz == 1, yes, ifelse(zz == 0, no, missing)))
  factor(out, levels = c(no, yes, missing))
}

lump_small_levels <- function(z, min_n = 30L, other_level = "Other/rare", missing_level = "Missing") {
  f <- factor_with_missing(z, missing_level = missing_level)
  tab <- table(f, useNA = "no")
  rare <- names(tab)[tab < min_n & names(tab) != missing_level]
  out <- as.character(f)
  out[out %in% rare] <- other_level
  droplevels(factor(out))
}

unit_source_group <- function(z) {
  txt <- clean_text(z)
  safe <- txt
  safe[is.na(safe)] <- ""
  out <- ifelse(grepl("Emergency", safe, ignore.case = TRUE), "Emergency Department",
         ifelse(grepl("Direct", safe, ignore.case = TRUE), "Direct Admit",
         ifelse(grepl("Floor|Acute", safe, ignore.case = TRUE), "Floor/Acute Care",
         ifelse(grepl("Other Hospital|Other ICU", safe, ignore.case = TRUE), "Transfer",
         ifelse(grepl("Operating|PACU|Recovery", safe, ignore.case = TRUE), "Perioperative",
         ifelse(is.na(txt), "Missing", "Other"))))))
  factor(out, levels = c(
    "Emergency Department", "Direct Admit", "Floor/Acute Care",
    "Transfer", "Perioperative", "Other", "Missing"
  ))
}

unit_type_group <- function(z) {
  txt <- clean_text(z)
  safe <- txt
  safe[is.na(safe)] <- ""
  out <- ifelse(safe == "Med-Surg ICU", "Med-Surg ICU",
         ifelse(safe == "MICU", "MICU",
         ifelse(safe == "SICU", "SICU",
         ifelse(grepl("Cardiac|CCU|CTICU|CSICU", safe, ignore.case = TRUE),
                "Cardiac/Surgical Cardiac ICU",
                ifelse(is.na(txt), "Missing", "Other ICU")))))
  factor(out, levels = c(
    "Med-Surg ICU", "MICU", "SICU",
    "Cardiac/Surgical Cardiac ICU", "Other ICU", "Missing"
  ))
}

read_db_table <- function() {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RPostgres", quietly = TRUE)) {
    stop("DB input requires packages DBI and RPostgres.", call. = FALSE)
  }
  pg_args <- list(
    dbname = Sys.getenv("EICU_PG_DBNAME", "eicu"),
    host = Sys.getenv("EICU_PG_HOST", "localhost"),
    port = as.integer(Sys.getenv("EICU_PG_PORT", "5432")),
    user = Sys.getenv("EICU_PG_USER", "postgres")
  )
  pg_password <- Sys.getenv("EICU_PG_PASSWORD", unset = NA_character_)
  if (!is.na(pg_password) && nzchar(pg_password)) pg_args$password <- pg_password
  con <- do.call(DBI::dbConnect, c(list(drv = RPostgres::Postgres()), pg_args))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(
    con,
    "SELECT * FROM public.eicu_formal_v1_analysis_main_model_ready_wide ORDER BY patientunitstayid"
  )
}

read_input_data <- function(input_source) {
  if (tolower(input_source) == "db") return(read_db_table())
  if (file.exists(input_source)) {
    return(read.csv(input_source, stringsAsFactors = FALSE, check.names = FALSE))
  }
  message("Input CSV not found: ", input_source)
  message("Trying DB input from public.eicu_formal_v1_analysis_main_model_ready_wide.")
  read_db_table()
}

count_one <- function(dat, var) {
  tab <- table(dat[[var]], useNA = "ifany")
  data.frame(
    variable = var,
    level = names(tab),
    n = as.integer(tab),
    stringsAsFactors = FALSE
  )
}

first_existing <- function(dat, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(dat)]
  if (length(hit) > 0) return(hit[1])
  if (required) {
    stop("Missing required column; tried: ", paste(candidates, collapse = ", "), call. = FALSE)
  }
  NA_character_
}

x <- read_input_data(input_source)
y <- x

if (!"arm2_model_ind" %in% names(y)) {
  y$arm2_model_ind <- 1L
}
if (!"abx_window_unit_t0" %in% names(y) && "arm2_abx_timing" %in% names(y)) {
  y$abx_window_unit_t0 <- as.character(y$arm2_abx_timing)
}

required_cols <- c(
  "patientunitstayid",
  "hospitalid",
  "death_30d_inhosp",
  "abx_window_unit_t0",
  "arm2_model_ind",
  "arm2_abx_timing",
  "arm2_late_3_12h",
  "hosp_main_three_window_bucket"
)

missing_required <- setdiff(required_cols, names(y))
if (length(missing_required) > 0) {
  stop("Missing required column(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
}

gender_col <- first_existing(y, c("gender", "gender_model"), required = TRUE)
ethnicity_col <- first_existing(y, c("ethnicity", "ethnicity_model"), required = TRUE)
unit_admit_col <- first_existing(y, c("unitadmitsource", "unit_admit_source_model"), required = TRUE)
unit_type_col <- first_existing(y, c("unittype", "unit_type_model"), required = TRUE)
hospital_admit_col <- first_existing(y, c("hospitaladmitsource", "hospital_admit_source_model"), required = FALSE)

y$gender_factor <- factor_with_missing(y[[gender_col]])
y$ethnicity_group <- lump_small_levels(y[[ethnicity_col]], min_n = 30L)
y$unit_admit_source_group <- unit_source_group(y[[unit_admit_col]])
y$hospital_admit_source_group <- if (!is.na(hospital_admit_col)) {
  unit_source_group(y[[hospital_admit_col]])
} else {
  factor(rep("Missing", nrow(y)), levels = c(
    "Emergency Department", "Direct Admit", "Floor/Acute Care",
    "Transfer", "Perioperative", "Other", "Missing"
  ))
}
y$unit_type_group <- unit_type_group(y[[unit_type_col]])
y$hosp_window_volume_bucket_factor <- factor(
  ifelse(is_miss(y$hosp_main_three_window_bucket), "Missing", as.character(y$hosp_main_three_window_bucket)),
  levels = c("0", "1-10", "11-30", ">30", "Missing")
)

y$abx_window_unit_t0_factor <- factor(
  ifelse(is_miss(y$abx_window_unit_t0), "Missing", as.character(y$abx_window_unit_t0)),
  levels = c("pre_t0_abx", "0-3h", "3-6h", "6-12h", ">12-72h", ">72h", "no_abx_record", "Missing")
)
y$arm2_abx_timing_factor <- factor(
  ifelse(is_miss(y$arm2_abx_timing), "Not arm2", as.character(y$arm2_abx_timing)),
  levels = c("0-3h", "3-12h", "Not arm2")
)
arm3_raw <- if ("arm3_abx_timing" %in% names(y)) y$arm3_abx_timing else rep(NA_character_, nrow(y))
y$arm3_abx_timing_factor <- factor(
  ifelse(is_miss(arm3_raw), "Not arm3", as.character(arm3_raw)),
  levels = c("0-3h", "3-6h", "6-12h", "Not arm3")
)
y$outcome_available_factor <- factor(
  ifelse(is.na(to_num(y$death_30d_inhosp)), "Outcome missing", "Outcome available"),
  levels = c("Outcome available", "Outcome missing")
)

for (nm in c(
  "vent_apache_clean",
  "vent_model",
  "intubated_apache_clean",
  "intubated_model",
  "dialysis_apache_clean",
  "dialysis_model",
  "diabetes_pred_clean",
  "diabetes_model",
  "cirrhosis_pred_clean",
  "cirrhosis_model",
  "immunosuppression_pred_clean",
  "immunosuppression_model",
  "hepaticfailure_pred_clean",
  "hepaticfailure_model",
  "metastaticcancer_pred_clean",
  "metastaticcancer_model"
)) {
  if (nm %in% names(y)) {
    y[[paste0(nm, "_factor")]] <- binary_factor(y[[nm]])
  }
}

categorical_vars <- c(
  "gender_factor",
  "ethnicity_group",
  "unit_admit_source_group",
  "hospital_admit_source_group",
  "unit_type_group",
  "hosp_window_volume_bucket_factor",
  "abx_window_unit_t0_factor",
  "arm2_abx_timing_factor",
  "arm3_abx_timing_factor",
  "outcome_available_factor",
  grep("_factor$", names(y), value = TRUE)
)
categorical_vars <- unique(categorical_vars[categorical_vars %in% names(y)])

counts <- do.call(rbind, lapply(categorical_vars, function(v) count_one(y, v)))

roles <- data.frame(
  variable = categorical_vars,
  role = ifelse(
    categorical_vars %in% c("abx_window_unit_t0_factor", "arm2_abx_timing_factor", "arm3_abx_timing_factor"),
    "exposure_or_analysis_arm",
    ifelse(categorical_vars == "outcome_available_factor", "outcome_availability_qc", "baseline_categorical")
  ),
  impute_target = FALSE,
  note = ifelse(
    categorical_vars == "hospital_admit_source_group",
    "High missingness in eICU; descriptive/sensitivity rather than main-model default.",
    ""
  ),
  stringsAsFactors = FALSE
)

write.csv(y, out_encoded, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(counts, out_counts, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(roles, out_roles, row.names = FALSE, fileEncoding = "UTF-8")

sink(out_report)
cat("eICU formal_v1 step 03 categorical encoding report\n")
cat("input_source:", input_source, "\n")
cat("rows:", nrow(y), "\n")
cat("distinct patientunitstayid:", length(unique(y$patientunitstayid)), "\n\n")

cat("arm2 QC:\n")
print(table(y$arm2_abx_timing_factor, useNA = "ifany"))
cat("\narm2 model rows:", sum(to_num(y$arm2_model_ind) == 1, na.rm = TRUE), "\n")

cat("\ncategorical variables:\n")
print(roles, row.names = FALSE)

cat("\noutputs:\n")
cat(out_encoded, "\n")
cat(out_counts, "\n")
cat(out_roles, "\n")
sink()

cat("Wrote:\n")
cat(out_encoded, "\n")
cat(out_counts, "\n")
cat(out_roles, "\n")
cat(out_report, "\n")
