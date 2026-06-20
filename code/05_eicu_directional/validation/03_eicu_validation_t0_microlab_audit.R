# eICU validation t0 feasibility audit using microLab culture offsets
# Scope: compare ICU-admission anchor vs culture-based anchors before Table 1/modeling.

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

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required.", call. = FALSE)
}

library(data.table)

print_section <- function(title) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
}

sql_literal <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

psql_exe <- Sys.getenv("PSQL_EXE", "D:/Physionet/postgresql/pgsql/bin/psql.exe")
pg_host <- Sys.getenv("PGHOST", "127.0.0.1")
pg_port <- Sys.getenv("PGPORT", "5431")
pg_user <- Sys.getenv("PGUSER", "postgres")
pg_db <- Sys.getenv("PGDATABASE", "eicu")

if (!file.exists(psql_exe)) {
  stop("psql executable not found. Set PSQL_EXE or check: ", psql_exe, call. = FALSE)
}

dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

out_anchor_summary <- file.path(base_dir, "eicu_v03_t0_anchor_summary.csv")
out_window_events <- file.path(base_dir, "eicu_v03_t0_anchor_window_events.csv")
out_culture_coverage <- file.path(base_dir, "eicu_v03_microlab_culture_coverage.csv")
out_culture_sites <- file.path(base_dir, "eicu_v03_microlab_culture_sites.csv")
out_report <- file.path(base_dir, "eicu_v03_t0_microlab_audit_report.txt")

sepsis_regex <- "\\m(sepsis|septic|septicemia|urosepsis)\\M"
explicit_sepsis_exclusion_regex <- "(signs and symptoms of sepsis|\\mSIRS\\M)"

abx_regex <- paste(
  c(
    "\\mvancomycin\\M", "\\mvancocin\\M", "\\mvanco\\M",
    "\\mcefepime\\M", "\\mceftriaxone\\M", "\\mceftazidime\\M", "\\mcefazolin\\M",
    "\\mpiperacillin\\M", "\\mtazobactam\\M", "\\mzosyn\\M",
    "\\mmeropenem\\M", "\\mimipenem\\M", "\\mertapenem\\M",
    "\\mazithromycin\\M",
    "\\mlevofloxacin\\M", "\\mlevaquin\\M", "\\mciprofloxacin\\M", "\\mmoxifloxacin\\M",
    "\\mampicillin\\M", "\\munasyn\\M",
    "\\mgentamicin\\M", "\\mtobramycin\\M", "\\mamikacin\\M",
    "\\mmetronidazole\\M", "\\mflagyl\\M",
    "\\mlinezolid\\M", "\\mdaptomycin\\M", "\\mclindamycin\\M", "\\mdoxycycline\\M"
  ),
  collapse = "|"
)

non_admin_drug_regex <- "\\m(consult|pharmacy|trough|level|random|peak|placeholder|protocol|monitor)\\M"
po_or_local_regex <- "\\m(PO|ORAL|TAB|TABS|TABLET|TABLETS|CAP|CAPS|CAPSULE|CAPSULES|SUSP|SUSPENSION|ELIXIR|OPHTH|OPHTHALMIC|OTIC|TOPICAL|CREAM|OINT|OINTMENT|NEB|NEBULIZER|INHAL|INHALATION|MOUTH|IRRIG|IRRIGATION|LOCK)\\M"

run_query <- function(sql) {
  sql <- sub(";\\s*$", "", sql)
  copy_sql <- paste0("copy (", sql, ") to stdout with csv header")
  sql_file <- tempfile(fileext = ".sql")
  writeLines(paste0(copy_sql, ";"), sql_file, useBytes = TRUE)
  on.exit(unlink(sql_file), add = TRUE)
  res <- system2(
    psql_exe,
    args = c(
      "--no-psqlrc",
      "-h", pg_host,
      "-p", pg_port,
      "-U", pg_user,
      "-d", pg_db,
      "-v", "ON_ERROR_STOP=1",
      "-P", "pager=off",
      "-q",
      "-f", sql_file
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    cat(paste(res, collapse = "\n"), "\n")
    stop("psql query failed with status ", status, call. = FALSE)
  }
  data.table::fread(text = paste(res, collapse = "\n"), showProgress = FALSE)
}

base_cte <- sprintf("
with sepsis_label_raw as (
  select
    patientunitstayid,
    diagnosisoffset as label_offset_min,
    diagnosisstring::text as label_text
  from eicu_crd.diagnosis
  where diagnosisstring ~* %s

  union all

  select
    patientunitstayid,
    admitdxenteredoffset as label_offset_min,
    concat_ws(' | ', admitdxpath, admitdxname, admitdxtext)::text as label_text
  from eicu_crd.admissiondx
  where admitdxpath ~* %s
     or coalesce(admitdxname, '') ~* %s
     or coalesce(admitdxtext, '') ~* %s
),
strict_sepsis as (
  select patientunitstayid
  from sepsis_label_raw
  group by patientunitstayid
  having bool_or(label_text ~* %s and label_text !~* %s)
),
abx_raw as (
  select
    patientunitstayid,
    drugstartoffset as abx_offset_min,
    drugname::text as drugname,
    'medication'::text as abx_source
  from eicu_crd.medication
  where drugname ~* %s
    and drugname !~* %s
    and drugname !~* %s
    and coalesce(drugordercancelled, '') !~* 'yes|true|cancel'
    and drugstartoffset is not null

  union all

  select
    patientunitstayid,
    infusionoffset as abx_offset_min,
    drugname::text as drugname,
    'infusiondrug'::text as abx_source
  from eicu_crd.infusiondrug
  where drugname ~* %s
    and drugname !~* %s
    and drugname !~* %s
    and infusionoffset is not null
),
first_abx as (
  select distinct on (patientunitstayid)
    patientunitstayid,
    abx_offset_min as first_abx_offset_min,
    drugname as first_abx_drugname,
    abx_source as first_abx_source
  from abx_raw
  order by patientunitstayid, abx_offset_min, abx_source, drugname
),
cohort_all as (
  select
    p.patientunitstayid,
    p.uniquepid,
    p.hospitalid,
    case
      when p.age ~ '^[0-9]+$' then p.age::int
      when p.age = '> 89' then 90
      else null
    end as age_num,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,
    p.unitdischargestatus,
    f.first_abx_offset_min,
    f.first_abx_drugname,
    f.first_abx_source,
    case
      when lower(coalesce(p.hospitaldischargestatus, '')) = 'expired'
           and p.hospitaldischargeoffset <= 43200 then 1
      when p.hospitaldischargestatus is null or p.hospitaldischargestatus = '' then null
      else 0
    end as death_30d_inhosp,
    row_number() over (
      partition by coalesce(nullif(p.uniquepid, ''), p.patientunitstayid::text)
      order by p.patientunitstayid
    ) as rn_patient
  from strict_sepsis s
  join eicu_crd.patient p using (patientunitstayid)
  left join first_abx f using (patientunitstayid)
  where
    case
      when p.age ~ '^[0-9]+$' then p.age::int
      when p.age = '> 89' then 90
      else null
    end >= 18
),
cohort as (
  select *
  from cohort_all
  where rn_patient = 1
),
culture_events as (
  select
    patientunitstayid,
    culturetakenoffset as culture_offset_min,
    culturesite,
    count(*) as microlab_rows_for_event
  from eicu_crd.microlab
  where culturetakenoffset is not null
  group by patientunitstayid, culturetakenoffset, culturesite
),
culture_by_window as (
  select patientunitstayid, culture_offset_min, culturesite, 'microlab_any_first'::text as anchor_definition
  from culture_events

  union all
  select patientunitstayid, culture_offset_min, culturesite, 'microlab_m24_p72_first'::text as anchor_definition
  from culture_events
  where culture_offset_min between -1440 and 4320

  union all
  select patientunitstayid, culture_offset_min, culturesite, 'microlab_m24_p24_first'::text as anchor_definition
  from culture_events
  where culture_offset_min between -1440 and 1440

  union all
  select patientunitstayid, culture_offset_min, culturesite, 'microlab_m6_p24_first'::text as anchor_definition
  from culture_events
  where culture_offset_min between -360 and 1440

  union all
  select patientunitstayid, culture_offset_min, culturesite, 'microlab_0_p72_first'::text as anchor_definition
  from culture_events
  where culture_offset_min between 0 and 4320
),
first_culture_anchor as (
  select distinct on (anchor_definition, patientunitstayid)
    anchor_definition,
    patientunitstayid,
    culture_offset_min as t0_offset_min,
    culturesite as t0_culture_site
  from culture_by_window
  order by anchor_definition, patientunitstayid, culture_offset_min, culturesite
),
anchor_grid as (
  select
    'unit_admission_offset0'::text as anchor_definition,
    c.patientunitstayid,
    c.uniquepid,
    c.hospitalid,
    c.first_abx_offset_min,
    c.first_abx_drugname,
    c.first_abx_source,
    c.death_30d_inhosp,
    0::integer as t0_offset_min,
    null::text as t0_culture_site,
    true as anchor_available
  from cohort c

  union all

  select
    defs.anchor_definition,
    c.patientunitstayid,
    c.uniquepid,
    c.hospitalid,
    c.first_abx_offset_min,
    c.first_abx_drugname,
    c.first_abx_source,
    c.death_30d_inhosp,
    a.t0_offset_min,
    a.t0_culture_site,
    (a.t0_offset_min is not null) as anchor_available
  from cohort c
  cross join (
    values
      ('microlab_any_first'::text),
      ('microlab_m24_p72_first'::text),
      ('microlab_m24_p24_first'::text),
      ('microlab_m6_p24_first'::text),
      ('microlab_0_p72_first'::text)
  ) defs(anchor_definition)
  left join first_culture_anchor a
    on a.patientunitstayid = c.patientunitstayid
   and a.anchor_definition = defs.anchor_definition
),
classified as (
  select
    *,
    case
      when not anchor_available then null
      when first_abx_offset_min is null then null
      else round(((first_abx_offset_min - t0_offset_min)::numeric / 60.0), 3)
    end as first_abx_delay_hours_from_anchor,
    case
      when not anchor_available then 'no_anchor'
      when first_abx_offset_min is null then 'no_abx_record'
      when first_abx_offset_min - t0_offset_min < 0 then 'pre_t0_abx'
      when first_abx_offset_min - t0_offset_min <= 180 then '0-3h'
      when first_abx_offset_min - t0_offset_min <= 360 then '3-6h'
      when first_abx_offset_min - t0_offset_min <= 720 then '6-12h'
      when first_abx_offset_min - t0_offset_min <= 4320 then '>12-72h'
      else '>72h'
    end as abx_window_from_anchor
  from anchor_grid
)
",
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(sepsis_regex),
sql_literal(explicit_sepsis_exclusion_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(po_or_local_regex),
sql_literal(abx_regex),
sql_literal(non_admin_drug_regex),
sql_literal(po_or_local_regex)
)

anchor_summary_sql <- paste0(base_cte, "
select
  anchor_definition,
  count(*) as cohort_n,
  sum(anchor_available::int) as anchor_available_n,
  round(100.0 * avg(anchor_available::int), 2) as anchor_available_pct,
  sum((death_30d_inhosp = 1)::int) as cohort_death_30d_inhosp_n,
  sum((anchor_available and death_30d_inhosp = 1)::int) as anchor_available_death_30d_inhosp_n,
  sum((abx_window_from_anchor = 'pre_t0_abx')::int) as pre_t0_abx_n,
  sum((abx_window_from_anchor = 'no_abx_record')::int) as no_abx_record_n,
  sum((abx_window_from_anchor in ('0-3h','3-6h','6-12h'))::int) as three_window_candidate_n,
  sum((abx_window_from_anchor = '0-3h')::int) as window_0_3h_n,
  sum((abx_window_from_anchor = '3-6h')::int) as window_3_6h_n,
  sum((abx_window_from_anchor = '6-12h')::int) as window_6_12h_n,
  sum((abx_window_from_anchor = '>12-72h')::int) as window_gt12_72h_n,
  sum((abx_window_from_anchor = '>72h')::int) as window_gt72h_n,
  sum((abx_window_from_anchor = 'no_anchor')::int) as no_anchor_n
from classified
group by anchor_definition
order by
  case anchor_definition
    when 'unit_admission_offset0' then 1
    when 'microlab_any_first' then 2
    when 'microlab_m24_p72_first' then 3
    when 'microlab_m24_p24_first' then 4
    when 'microlab_m6_p24_first' then 5
    when 'microlab_0_p72_first' then 6
    else 99
  end
")

window_events_sql <- paste0(base_cte, "
select
  anchor_definition,
  abx_window_from_anchor,
  count(*) as stays,
  sum((death_30d_inhosp = 1)::int) as death_30d_inhosp_n,
  round(100.0 * avg((death_30d_inhosp = 1)::int), 2) as death_30d_inhosp_pct
from classified
group by anchor_definition, abx_window_from_anchor
order by
  case anchor_definition
    when 'unit_admission_offset0' then 1
    when 'microlab_any_first' then 2
    when 'microlab_m24_p72_first' then 3
    when 'microlab_m24_p24_first' then 4
    when 'microlab_m6_p24_first' then 5
    when 'microlab_0_p72_first' then 6
    else 99
  end,
  case abx_window_from_anchor
    when 'no_anchor' then 0
    when 'pre_t0_abx' then 1
    when '0-3h' then 2
    when '3-6h' then 3
    when '6-12h' then 4
    when '>12-72h' then 5
    when '>72h' then 6
    when 'no_abx_record' then 7
    else 99
  end
")

culture_coverage_sql <- paste0(base_cte, "
select
  'strict_explicit_sepsis_exclude_po_local_first_stay'::text as cohort_scope,
  count(distinct c.patientunitstayid) as cohort_n,
  count(distinct c.patientunitstayid) filter (where ce.patientunitstayid is not null) as any_culture_stays,
  round(100.0 * count(distinct c.patientunitstayid) filter (where ce.patientunitstayid is not null) / count(distinct c.patientunitstayid), 2) as any_culture_stay_pct,
  count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between -1440 and 4320) as culture_m24_p72_stays,
  round(100.0 * count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between -1440 and 4320) / count(distinct c.patientunitstayid), 2) as culture_m24_p72_pct,
  count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between -1440 and 1440) as culture_m24_p24_stays,
  round(100.0 * count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between -1440 and 1440) / count(distinct c.patientunitstayid), 2) as culture_m24_p24_pct,
  count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between -360 and 1440) as culture_m6_p24_stays,
  round(100.0 * count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between -360 and 1440) / count(distinct c.patientunitstayid), 2) as culture_m6_p24_pct,
  count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between 0 and 4320) as culture_0_p72_stays,
  round(100.0 * count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between 0 and 4320) / count(distinct c.patientunitstayid), 2) as culture_0_p72_pct,
  count(ce.*) as distinct_culture_events,
  min(ce.culture_offset_min) as min_culture_offset_min,
  percentile_cont(0.25) within group (order by ce.culture_offset_min) as p25_culture_offset_min,
  percentile_cont(0.5) within group (order by ce.culture_offset_min) as median_culture_offset_min,
  percentile_cont(0.75) within group (order by ce.culture_offset_min) as p75_culture_offset_min,
  max(ce.culture_offset_min) as max_culture_offset_min
from cohort c
left join culture_events ce using (patientunitstayid)
")

culture_sites_sql <- paste0(base_cte, "
select
  coalesce(ce.culturesite, 'NO_CULTURE') as culturesite,
  count(*) as culture_events_or_no_culture_rows,
  count(distinct c.patientunitstayid) as stays,
  count(distinct c.patientunitstayid) filter (where ce.culture_offset_min between -1440 and 4320) as stays_m24_p72,
  min(ce.culture_offset_min) as min_offset_min,
  max(ce.culture_offset_min) as max_offset_min
from cohort c
left join culture_events ce using (patientunitstayid)
group by coalesce(ce.culturesite, 'NO_CULTURE')
order by stays desc, culture_events_or_no_culture_rows desc
limit 50
")

print_section("Run t0 anchor summary")
anchor_summary <- run_query(anchor_summary_sql)
setDT(anchor_summary)

print_section("Run t0 anchor window events")
window_events <- run_query(window_events_sql)
setDT(window_events)

print_section("Run culture coverage")
culture_coverage <- run_query(culture_coverage_sql)
setDT(culture_coverage)

print_section("Run culture site table")
culture_sites <- run_query(culture_sites_sql)
setDT(culture_sites)

fwrite(anchor_summary, out_anchor_summary)
fwrite(window_events, out_window_events)
fwrite(culture_coverage, out_culture_coverage)
fwrite(culture_sites, out_culture_sites)

report_lines <- c(
  "eICU microLab t0 feasibility audit",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Database: ", pg_db, " / schema eicu_crd"),
  paste0("Base directory: ", base_dir),
  "",
  "Cohort:",
  "- strict explicit sepsis label",
  "- adult",
  "- first qualifying stay per uniquepid",
  "- antibiotics exclude non-administration terms and PO/local route terms",
  "",
  "Anchor definitions:",
  "- unit_admission_offset0: ICU admission offset 0",
  "- microlab_any_first: first available microLab culture offset",
  "- microlab_m24_p72_first: first culture from -24h to +72h around ICU admission",
  "- microlab_m24_p24_first: first culture from -24h to +24h",
  "- microlab_m6_p24_first: first culture from -6h to +24h",
  "- microlab_0_p72_first: first culture from 0 to +72h",
  "",
  "Anchor summary:",
  paste(capture.output(print(anchor_summary)), collapse = "\n"),
  "",
  "Culture coverage:",
  paste(capture.output(print(culture_coverage)), collapse = "\n"),
  "",
  "Window events:",
  paste(capture.output(print(window_events)), collapse = "\n")
)

writeLines(report_lines, out_report, useBytes = TRUE)

print_section("Done")
cat("Wrote:\n")
cat(" - ", out_anchor_summary, "\n", sep = "")
cat(" - ", out_window_events, "\n", sep = "")
cat(" - ", out_culture_coverage, "\n", sep = "")
cat(" - ", out_culture_sites, "\n", sep = "")
cat(" - ", out_report, "\n", sep = "")
