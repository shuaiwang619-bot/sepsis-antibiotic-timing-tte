# eICU validation covariate coverage audit
# Scope: coverage and source diagnostics only. This script does not create the
# final model-ready table.

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

out_coverage <- file.path(base_dir, "eicu_v04_covariate_coverage_summary.csv")
out_value_diag <- file.path(base_dir, "eicu_v04_covariate_value_diagnostics.csv")
out_record_diag <- file.path(base_dir, "eicu_v04_apache_record_diagnostics.csv")
out_lab_units <- file.path(base_dir, "eicu_v04_lab_unit_diagnostics.csv")
out_vital_sources <- file.path(base_dir, "eicu_v04_vital_source_diagnostics.csv")
out_vasopressor_terms <- file.path(base_dir, "eicu_v04_vasopressor_terms_0_24h.csv")
out_report <- file.path(base_dir, "eicu_v04_covariate_coverage_audit_report.txt")

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
vasopressor_regex <- "\\m(norepinephrine|levophed|epinephrine|vasopressin|phenylephrine|neosynephrine|neo-synephrine|neo synephrine|dopamine)\\M"

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
    p.gender,
    p.ethnicity,
    p.hospitaladmitsource,
    p.unitadmitsource,
    p.unittype,
    p.unitstaytype,
    p.apacheadmissiondx,
    p.hospitaladmitoffset,
    p.admissionheight,
    p.admissionweight,
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
  select
    *,
    case
      when first_abx_offset_min is null then 'no_abx_record'
      when first_abx_offset_min < 0 then 'pre_t0_abx'
      when first_abx_offset_min <= 180 then '0-3h'
      when first_abx_offset_min <= 360 then '3-6h'
      when first_abx_offset_min <= 720 then '6-12h'
      when first_abx_offset_min <= 4320 then '>12-72h'
      else '>72h'
    end as abx_window_unit_t0
  from cohort_all
  where rn_patient = 1
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

feature_cte <- sprintf("
, apr as (
  select
    patientunitstayid,
    count(*) as apr_rows,
    max((apacheversion = 'IV')::int) as has_apache_iv,
    max((apacheversion = 'IVa')::int) as has_apache_iva,
    max(acutephysiologyscore) filter (where apacheversion = 'IVa') as aps_iva,
    max(apachescore) filter (where apacheversion = 'IVa') as apache_score_iva,
    max(predictedhospitalmortality) filter (where apacheversion = 'IVa') as pred_hosp_mort_iva,
    max(predictedicumortality) filter (where apacheversion = 'IVa') as pred_icu_mort_iva
  from eicu_crd.apachepatientresult
  group by patientunitstayid
),
aps as (
  select
    patientunitstayid,
    count(*) as aps_rows,
    max(intubated) as intubated_apache,
    max(vent) as vent_apache,
    max(dialysis) as dialysis_apache,
    max(eyes) as eyes_apache,
    max(motor) as motor_apache,
    max(verbal) as verbal_apache,
    max(meds) as meds_apache,
    max(urine) as urine_apache,
    max(wbc) as wbc_apache,
    max(temperature) as temperature_apache,
    max(respiratoryrate) as respiratoryrate_apache,
    max(sodium) as sodium_apache,
    max(heartrate) as heartrate_apache,
    max(meanbp) as meanbp_apache,
    max(ph) as ph_apache,
    max(hematocrit) as hematocrit_apache,
    max(creatinine) as creatinine_apache,
    max(albumin) as albumin_apache,
    max(pao2) as pao2_apache,
    max(pco2) as pco2_apache,
    max(bun) as bun_apache,
    max(glucose) as glucose_apache,
    max(bilirubin) as bilirubin_apache,
    max(fio2) as fio2_apache
  from eicu_crd.apacheapsvar
  group by patientunitstayid
),
pred as (
  select
    patientunitstayid,
    count(*) as pred_rows,
    max(aids) as aids_pred,
    max(hepaticfailure) as hepaticfailure_pred,
    max(lymphoma) as lymphoma_pred,
    max(metastaticcancer) as metastaticcancer_pred,
    max(leukemia) as leukemia_pred,
    max(immunosuppression) as immunosuppression_pred,
    max(cirrhosis) as cirrhosis_pred,
    max(diabetes) as diabetes_pred,
    max(electivesurgery) as electivesurgery_pred,
    max(readmit) as readmit_pred,
    max(ventday1) as ventday1_pred,
    max(oobventday1) as oobventday1_pred,
    max(oobintubday1) as oobintubday1_pred,
    max(day1meds) as day1meds_pred,
    max(day1verbal) as day1verbal_pred,
    max(day1motor) as day1motor_pred,
    max(day1eyes) as day1eyes_pred,
    max(day1pao2) as day1pao2_pred,
    max(day1fio2) as day1fio2_pred
  from eicu_crd.apachepredvar
  group by patientunitstayid
),
lab_flags as (
  select
    l.patientunitstayid,
    max((lower(l.labname) = 'creatinine' and l.labresultoffset between 0 and 1440)::int) as lab_creatinine_0_24,
    max((lower(l.labname) = 'creatinine' and l.labresultoffset between -360 and 1440)::int) as lab_creatinine_m6_24,
    max((lower(l.labname) = 'total bilirubin' and l.labresultoffset between 0 and 1440)::int) as lab_bilirubin_0_24,
    max((lower(l.labname) = 'total bilirubin' and l.labresultoffset between -360 and 1440)::int) as lab_bilirubin_m6_24,
    max((lower(l.labname) = 'lactate' and l.labresultoffset between 0 and 1440)::int) as lab_lactate_0_24,
    max((lower(l.labname) = 'lactate' and l.labresultoffset between -360 and 1440)::int) as lab_lactate_m6_24,
    max((lower(l.labname) = 'platelets x 1000' and l.labresultoffset between 0 and 1440)::int) as lab_platelets_0_24,
    max((lower(l.labname) = 'wbc x 1000' and l.labresultoffset between 0 and 1440)::int) as lab_wbc_0_24,
    max((lower(l.labname) = 'pao2' and l.labresultoffset between 0 and 1440)::int) as lab_pao2_0_24,
    max((lower(l.labname) = 'fio2' and l.labresultoffset between 0 and 1440)::int) as lab_fio2_0_24
  from eicu_crd.lab l
  join cohort c using (patientunitstayid)
  where l.labresult is not null
    and lower(l.labname) in ('creatinine','total bilirubin','lactate','platelets x 1000','wbc x 1000','pao2','fio2')
    and l.labresultoffset between -360 and 1440
  group by l.patientunitstayid
),
vp as (
  select
    v.patientunitstayid,
    count(*) filter (where v.systemicmean is not null and v.observationoffset between 0 and 1440) as vitalperiodic_meanbp_rows_0_24,
    min(v.systemicmean) filter (where v.systemicmean is not null and v.observationoffset between 0 and 1440) as meanbp_min_vitalperiodic_0_24,
    count(*) filter (where v.heartrate is not null and v.observationoffset between 0 and 1440) as vitalperiodic_hr_rows_0_24,
    max(v.heartrate) filter (where v.heartrate is not null and v.observationoffset between 0 and 1440) as heartrate_max_vitalperiodic_0_24,
    count(*) filter (where v.respiration is not null and v.observationoffset between 0 and 1440) as vitalperiodic_rr_rows_0_24,
    max(v.respiration) filter (where v.respiration is not null and v.observationoffset between 0 and 1440) as respiration_max_vitalperiodic_0_24,
    count(*) filter (where v.sao2 is not null and v.observationoffset between 0 and 1440) as vitalperiodic_sao2_rows_0_24,
    min(v.sao2) filter (where v.sao2 is not null and v.observationoffset between 0 and 1440) as sao2_min_vitalperiodic_0_24
  from eicu_crd.vitalperiodic v
  join cohort c using (patientunitstayid)
  where v.observationoffset between 0 and 1440
  group by v.patientunitstayid
),
va as (
  select
    v.patientunitstayid,
    count(*) filter (where v.noninvasivemean is not null and v.observationoffset between 0 and 1440) as vitalaperiodic_meanbp_rows_0_24,
    min(v.noninvasivemean) filter (where v.noninvasivemean is not null and v.observationoffset between 0 and 1440) as meanbp_min_vitalaperiodic_0_24
  from eicu_crd.vitalaperiodic v
  join cohort c using (patientunitstayid)
  where v.observationoffset between 0 and 1440
  group by v.patientunitstayid
),
pressor as (
  select
    i.patientunitstayid,
    count(*) as vasopressor_rows_0_24
  from eicu_crd.infusiondrug i
  join cohort c using (patientunitstayid)
  where i.infusionoffset between 0 and 1440
    and i.drugname ~* %s
  group by i.patientunitstayid
),
io as (
  select
    io.patientunitstayid,
    count(*) filter (where io.intakeoutputoffset between 0 and 1440) as intakeoutput_rows_0_24,
    max(io.outputtotal) filter (where io.intakeoutputoffset between 0 and 1440 and io.outputtotal is not null) as outputtotal_max_0_24,
    sum(io.cellvaluenumeric) filter (
      where io.intakeoutputoffset between 0 and 1440
        and io.cellvaluenumeric is not null
        and concat_ws(' ', io.cellpath, io.celllabel, io.cellvaluetext) ~* 'urine'
    ) as urine_cell_sum_0_24
  from eicu_crd.intakeoutput io
  join cohort c using (patientunitstayid)
  where io.intakeoutputoffset between 0 and 1440
  group by io.patientunitstayid
),
feature as (
  select
    c.*,
    apr.*,
    aps.*,
    pred.*,
    lab_flags.*,
    vp.*,
    va.*,
    pressor.vasopressor_rows_0_24,
    io.intakeoutput_rows_0_24,
    io.outputtotal_max_0_24,
    io.urine_cell_sum_0_24
  from cohort c
  left join apr using (patientunitstayid)
  left join aps using (patientunitstayid)
  left join pred using (patientunitstayid)
  left join lab_flags using (patientunitstayid)
  left join vp using (patientunitstayid)
  left join va using (patientunitstayid)
  left join pressor using (patientunitstayid)
  left join io using (patientunitstayid)
)
",
sql_literal(vasopressor_regex)
)

coverage_sql <- paste0(base_cte, feature_cte, "
select *
from (
  select 'age_num','patient','demographic','baseline','age parsed from patient.age', sum((age_num is not null)::int), count(*)
  from feature

  union all

  select 'gender','patient','demographic','baseline','categorical', sum((gender is not null and gender <> '')::int), count(*)
  from feature

  union all

  select 'ethnicity','patient','demographic','baseline','categorical', sum((ethnicity is not null and ethnicity <> '')::int), count(*)
  from feature

  union all

  select 'hospitaladmitsource','patient','admission','baseline','categorical', sum((hospitaladmitsource is not null and hospitaladmitsource <> '')::int), count(*)
  from feature

  union all

  select 'unitadmitsource','patient','admission','baseline','categorical', sum((unitadmitsource is not null and unitadmitsource <> '')::int), count(*)
  from feature

  union all

  select 'unittype','patient','admission','baseline','categorical', sum((unittype is not null and unittype <> '')::int), count(*)
  from feature

  union all

  select 'unitstaytype','patient','admission','baseline','categorical', sum((unitstaytype is not null and unitstaytype <> '')::int), count(*)
  from feature

  union all

  select 'apacheadmissiondx','patient','admission_diagnosis','baseline','text diagnosis, descriptive only unless grouped later', sum((apacheadmissiondx is not null and apacheadmissiondx <> '')::int), count(*)
  from feature

  union all

  select 'admissionheight','patient','anthropometric','baseline','cm; range audit needed before modeling', sum((admissionheight is not null)::int), count(*)
  from feature

  union all

  select 'admissionweight','patient','anthropometric','baseline','kg; range audit needed before modeling', sum((admissionweight is not null)::int), count(*)
  from feature

  union all

  select 'death_30d_inhosp','patient','outcome','30d','30-day in-hospital mortality', sum((death_30d_inhosp is not null)::int), count(*)
  from feature

  union all

  select 'abx_window_unit_t0','derived','exposure','ICU admission t0','mutually exclusive window', sum((abx_window_unit_t0 is not null)::int), count(*)
  from feature

  union all

  select 'first_abx_offset_min','derived','exposure','ICU admission t0','first systemic antibiotic offset', sum((first_abx_offset_min is not null)::int), count(*)
  from feature

  union all

  select 'aps_iva','apachepatientresult','severity_score','APACHE IVa','acute physiology score', sum((aps_iva is not null)::int), count(*)
  from feature

  union all

  select 'apache_score_iva','apachepatientresult','severity_score','APACHE IVa','APACHE score', sum((apache_score_iva is not null)::int), count(*)
  from feature

  union all

  select 'pred_hosp_mort_iva','apachepatientresult','risk_prediction','APACHE IVa','predicted hospital mortality, not a causal covariate by default', sum((pred_hosp_mort_iva is not null and pred_hosp_mort_iva <> '')::int), count(*)
  from feature

  union all

  select 'vent_apache','apacheapsvar','respiratory_support','APACHE day1','binary APACHE variable', sum((vent_apache is not null)::int), count(*)
  from feature

  union all

  select 'intubated_apache','apacheapsvar','respiratory_support','APACHE day1','binary APACHE variable', sum((intubated_apache is not null)::int), count(*)
  from feature

  union all

  select 'dialysis_apache','apacheapsvar','renal_support','APACHE day1','binary APACHE variable', sum((dialysis_apache is not null)::int), count(*)
  from feature

  union all

  select 'eyes_apache','apacheapsvar','neurologic','APACHE day1','GCS eye component', sum((eyes_apache is not null)::int), count(*)
  from feature

  union all

  select 'motor_apache','apacheapsvar','neurologic','APACHE day1','GCS motor component', sum((motor_apache is not null)::int), count(*)
  from feature

  union all

  select 'verbal_apache','apacheapsvar','neurologic','APACHE day1','GCS verbal component', sum((verbal_apache is not null)::int), count(*)
  from feature

  union all

  select 'meds_apache','apacheapsvar','neurologic','APACHE day1','sedation/meds APACHE variable', sum((meds_apache is not null)::int), count(*)
  from feature

  union all

  select 'urine_apache','apacheapsvar','renal','APACHE day1','urine output APACHE variable', sum((urine_apache is not null)::int), count(*)
  from feature

  union all

  select 'wbc_apache','apacheapsvar','lab','APACHE day1','APACHE extracted WBC', sum((wbc_apache is not null)::int), count(*)
  from feature

  union all

  select 'temperature_apache','apacheapsvar','vital','APACHE day1','APACHE extracted temperature', sum((temperature_apache is not null)::int), count(*)
  from feature

  union all

  select 'respiratoryrate_apache','apacheapsvar','vital','APACHE day1','APACHE extracted respiratory rate', sum((respiratoryrate_apache is not null)::int), count(*)
  from feature

  union all

  select 'sodium_apache','apacheapsvar','lab','APACHE day1','APACHE extracted sodium', sum((sodium_apache is not null)::int), count(*)
  from feature

  union all

  select 'heartrate_apache','apacheapsvar','vital','APACHE day1','APACHE extracted heart rate', sum((heartrate_apache is not null)::int), count(*)
  from feature

  union all

  select 'meanbp_apache','apacheapsvar','vital','APACHE day1','APACHE extracted mean BP', sum((meanbp_apache is not null)::int), count(*)
  from feature

  union all

  select 'ph_apache','apacheapsvar','lab','APACHE day1','APACHE extracted pH', sum((ph_apache is not null)::int), count(*)
  from feature

  union all

  select 'hematocrit_apache','apacheapsvar','lab','APACHE day1','APACHE extracted hematocrit', sum((hematocrit_apache is not null)::int), count(*)
  from feature

  union all

  select 'creatinine_apache','apacheapsvar','lab','APACHE day1','APACHE extracted creatinine', sum((creatinine_apache is not null)::int), count(*)
  from feature

  union all

  select 'albumin_apache','apacheapsvar','lab','APACHE day1','APACHE extracted albumin', sum((albumin_apache is not null)::int), count(*)
  from feature

  union all

  select 'pao2_apache','apacheapsvar','lab','APACHE day1','APACHE extracted PaO2', sum((pao2_apache is not null)::int), count(*)
  from feature

  union all

  select 'pco2_apache','apacheapsvar','lab','APACHE day1','APACHE extracted PaCO2', sum((pco2_apache is not null)::int), count(*)
  from feature

  union all

  select 'bun_apache','apacheapsvar','lab','APACHE day1','APACHE extracted BUN', sum((bun_apache is not null)::int), count(*)
  from feature

  union all

  select 'glucose_apache','apacheapsvar','lab','APACHE day1','APACHE extracted glucose', sum((glucose_apache is not null)::int), count(*)
  from feature

  union all

  select 'bilirubin_apache','apacheapsvar','lab','APACHE day1','APACHE extracted bilirubin', sum((bilirubin_apache is not null)::int), count(*)
  from feature

  union all

  select 'fio2_apache','apacheapsvar','respiratory','APACHE day1','APACHE extracted FiO2', sum((fio2_apache is not null)::int), count(*)
  from feature

  union all

  select 'diabetes_pred','apachepredvar','comorbidity','baseline','APACHE prediction variable', sum((diabetes_pred is not null)::int), count(*)
  from feature

  union all

  select 'immunosuppression_pred','apachepredvar','comorbidity','baseline','APACHE prediction variable', sum((immunosuppression_pred is not null)::int), count(*)
  from feature

  union all

  select 'cirrhosis_pred','apachepredvar','comorbidity','baseline','APACHE prediction variable', sum((cirrhosis_pred is not null)::int), count(*)
  from feature

  union all

  select 'hepaticfailure_pred','apachepredvar','comorbidity','baseline','APACHE prediction variable', sum((hepaticfailure_pred is not null)::int), count(*)
  from feature

  union all

  select 'metastaticcancer_pred','apachepredvar','comorbidity','baseline','APACHE prediction variable', sum((metastaticcancer_pred is not null)::int), count(*)
  from feature

  union all

  select 'electivesurgery_pred','apachepredvar','admission','baseline','APACHE prediction variable', sum((electivesurgery_pred is not null)::int), count(*)
  from feature

  union all

  select 'readmit_pred','apachepredvar','admission','baseline','APACHE prediction variable', sum((readmit_pred is not null)::int), count(*)
  from feature

  union all

  select 'day1meds_pred','apachepredvar','neurologic','APACHE day1','APACHE prediction variable', sum((day1meds_pred is not null)::int), count(*)
  from feature

  union all

  select 'day1eyes_pred','apachepredvar','neurologic','APACHE day1','APACHE prediction variable', sum((day1eyes_pred is not null)::int), count(*)
  from feature

  union all

  select 'day1motor_pred','apachepredvar','neurologic','APACHE day1','APACHE prediction variable', sum((day1motor_pred is not null)::int), count(*)
  from feature

  union all

  select 'day1verbal_pred','apachepredvar','neurologic','APACHE day1','APACHE prediction variable', sum((day1verbal_pred is not null)::int), count(*)
  from feature

  union all

  select 'lab_creatinine_0_24','lab','lab_raw_coverage','0-24h','raw lab coverage flag', sum(coalesce(lab_creatinine_0_24,0)), count(*)
  from feature

  union all

  select 'lab_bilirubin_0_24','lab','lab_raw_coverage','0-24h','raw lab coverage flag', sum(coalesce(lab_bilirubin_0_24,0)), count(*)
  from feature

  union all

  select 'lab_lactate_0_24','lab','lab_raw_coverage','0-24h','raw lab coverage flag', sum(coalesce(lab_lactate_0_24,0)), count(*)
  from feature

  union all

  select 'lab_platelets_0_24','lab','lab_raw_coverage','0-24h','raw lab coverage flag', sum(coalesce(lab_platelets_0_24,0)), count(*)
  from feature

  union all

  select 'lab_wbc_0_24','lab','lab_raw_coverage','0-24h','raw lab coverage flag', sum(coalesce(lab_wbc_0_24,0)), count(*)
  from feature

  union all

  select 'lab_pao2_0_24','lab','lab_raw_coverage','0-24h','raw lab coverage flag', sum(coalesce(lab_pao2_0_24,0)), count(*)
  from feature

  union all

  select 'lab_fio2_0_24','lab','lab_raw_coverage','0-24h','raw lab coverage flag', sum(coalesce(lab_fio2_0_24,0)), count(*)
  from feature

  union all

  select 'lab_lactate_m6_24','lab','lab_raw_coverage','-6h to +24h','raw lab coverage flag', sum(coalesce(lab_lactate_m6_24,0)), count(*)
  from feature

  union all

  select 'meanbp_min_vitalperiodic_0_24','vitalperiodic','vital_raw_coverage','0-24h','systemicmean minimum', sum((meanbp_min_vitalperiodic_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'meanbp_min_vitalaperiodic_0_24','vitalaperiodic','vital_raw_coverage','0-24h','noninvasivemean minimum', sum((meanbp_min_vitalaperiodic_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'heartrate_max_vitalperiodic_0_24','vitalperiodic','vital_raw_coverage','0-24h','heart rate maximum', sum((heartrate_max_vitalperiodic_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'respiration_max_vitalperiodic_0_24','vitalperiodic','vital_raw_coverage','0-24h','respiration maximum', sum((respiration_max_vitalperiodic_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'sao2_min_vitalperiodic_0_24','vitalperiodic','vital_raw_coverage','0-24h','SaO2 minimum', sum((sao2_min_vitalperiodic_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'vasopressor_rows_0_24','infusiondrug','shock_proxy','0-24h','presence means candidate vasopressor record; absence not assumed normal in this audit', sum((vasopressor_rows_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'intakeoutput_rows_0_24','intakeoutput','urine_raw_coverage','0-24h','any intakeoutput rows', sum((intakeoutput_rows_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'outputtotal_max_0_24','intakeoutput','urine_raw_coverage','0-24h','outputtotal available', sum((outputtotal_max_0_24 is not null)::int), count(*)
  from feature

  union all

  select 'urine_cell_sum_0_24','intakeoutput','urine_raw_coverage','0-24h','urine-labeled cell values summed; exploratory only', sum((urine_cell_sum_0_24 is not null)::int), count(*)
  from feature
) as v(variable, source, role, time_window, note, available_n, cohort_n)
cross join lateral (
  select
    (cohort_n - available_n) as missing_n,
    round((100.0 * available_n / nullif(cohort_n, 0))::numeric, 2) as coverage_pct,
    case
      when available_n = cohort_n then 'complete'
      when (100.0 * available_n / nullif(cohort_n, 0)) >= 90 then 'main_candidate'
      when (100.0 * available_n / nullif(cohort_n, 0)) >= 70 then 'possible_sensitivity'
      when (100.0 * available_n / nullif(cohort_n, 0)) >= 30 then 'descriptive_or_sensitivity_only'
      else 'too_sparse_for_main'
    end as coverage_decision
) calc
order by role, source, variable
")

value_diag_sql <- paste0(base_cte, feature_cte, "
, long_values as (
  select 'age_num'::text as variable, age_num::numeric as value from feature where age_num is not null
  union all select 'admissionheight', admissionheight::numeric from feature where admissionheight is not null
  union all select 'admissionweight', admissionweight::numeric from feature where admissionweight is not null
  union all select 'aps_iva', aps_iva::numeric from feature where aps_iva is not null
  union all select 'apache_score_iva', apache_score_iva::numeric from feature where apache_score_iva is not null
  union all select 'urine_apache', urine_apache::numeric from feature where urine_apache is not null
  union all select 'wbc_apache', wbc_apache::numeric from feature where wbc_apache is not null
  union all select 'temperature_apache', temperature_apache::numeric from feature where temperature_apache is not null
  union all select 'respiratoryrate_apache', respiratoryrate_apache::numeric from feature where respiratoryrate_apache is not null
  union all select 'sodium_apache', sodium_apache::numeric from feature where sodium_apache is not null
  union all select 'heartrate_apache', heartrate_apache::numeric from feature where heartrate_apache is not null
  union all select 'meanbp_apache', meanbp_apache::numeric from feature where meanbp_apache is not null
  union all select 'ph_apache', ph_apache::numeric from feature where ph_apache is not null
  union all select 'hematocrit_apache', hematocrit_apache::numeric from feature where hematocrit_apache is not null
  union all select 'creatinine_apache', creatinine_apache::numeric from feature where creatinine_apache is not null
  union all select 'albumin_apache', albumin_apache::numeric from feature where albumin_apache is not null
  union all select 'pao2_apache', pao2_apache::numeric from feature where pao2_apache is not null
  union all select 'pco2_apache', pco2_apache::numeric from feature where pco2_apache is not null
  union all select 'bun_apache', bun_apache::numeric from feature where bun_apache is not null
  union all select 'glucose_apache', glucose_apache::numeric from feature where glucose_apache is not null
  union all select 'bilirubin_apache', bilirubin_apache::numeric from feature where bilirubin_apache is not null
  union all select 'fio2_apache', fio2_apache::numeric from feature where fio2_apache is not null
  union all select 'meanbp_min_vitalperiodic_0_24', meanbp_min_vitalperiodic_0_24::numeric from feature where meanbp_min_vitalperiodic_0_24 is not null
  union all select 'meanbp_min_vitalaperiodic_0_24', meanbp_min_vitalaperiodic_0_24::numeric from feature where meanbp_min_vitalaperiodic_0_24 is not null
  union all select 'heartrate_max_vitalperiodic_0_24', heartrate_max_vitalperiodic_0_24::numeric from feature where heartrate_max_vitalperiodic_0_24 is not null
  union all select 'respiration_max_vitalperiodic_0_24', respiration_max_vitalperiodic_0_24::numeric from feature where respiration_max_vitalperiodic_0_24 is not null
  union all select 'sao2_min_vitalperiodic_0_24', sao2_min_vitalperiodic_0_24::numeric from feature where sao2_min_vitalperiodic_0_24 is not null
  union all select 'outputtotal_max_0_24', outputtotal_max_0_24::numeric from feature where outputtotal_max_0_24 is not null
  union all select 'urine_cell_sum_0_24', urine_cell_sum_0_24::numeric from feature where urine_cell_sum_0_24 is not null
),
cohort_n as (select count(*) as n from feature)
select
  variable,
  (select n from cohort_n) as cohort_n,
  count(value) as nonmissing_n,
  (select n from cohort_n) - count(value) as missing_n,
  round((100.0 * count(value) / nullif((select n from cohort_n), 0))::numeric, 2) as coverage_pct,
  min(value) as min_value,
  percentile_cont(0.01) within group (order by value) as p01,
  percentile_cont(0.25) within group (order by value) as p25,
  percentile_cont(0.5) within group (order by value) as median,
  percentile_cont(0.75) within group (order by value) as p75,
  percentile_cont(0.99) within group (order by value) as p99,
  max(value) as max_value
from long_values
group by variable
order by variable
")

record_diag_sql <- paste0(base_cte, "
select *
from (
  select
    'apachepatientresult'::text as source_table,
    'all_versions'::text as stratum,
    count(*) as rows_n,
    count(distinct patientunitstayid) as stays_n,
    count(*) - count(distinct patientunitstayid) as extra_rows_n
  from eicu_crd.apachepatientresult
  where patientunitstayid in (select patientunitstayid from cohort)

  union all

  select
    'apachepatientresult',
    apacheversion,
    count(*),
    count(distinct patientunitstayid),
    count(*) - count(distinct patientunitstayid)
  from eicu_crd.apachepatientresult
  where patientunitstayid in (select patientunitstayid from cohort)
  group by apacheversion

  union all

  select
    'apacheapsvar',
    'all_rows',
    count(*),
    count(distinct patientunitstayid),
    count(*) - count(distinct patientunitstayid)
  from eicu_crd.apacheapsvar
  where patientunitstayid in (select patientunitstayid from cohort)

  union all

  select
    'apachepredvar',
    'all_rows',
    count(*),
    count(distinct patientunitstayid),
    count(*) - count(distinct patientunitstayid)
  from eicu_crd.apachepredvar
  where patientunitstayid in (select patientunitstayid from cohort)
) x
order by source_table, stratum
")

lab_units_sql <- paste0(base_cte, "
select
  l.labname,
  coalesce(l.labmeasurenamesystem, '') as lab_unit,
  count(*) as lab_rows_m6_24,
  count(distinct l.patientunitstayid) as stays_m6_24,
  count(*) filter (where l.labresultoffset between 0 and 1440) as lab_rows_0_24,
  count(distinct l.patientunitstayid) filter (where l.labresultoffset between 0 and 1440) as stays_0_24,
  min(l.labresult) as min_value,
  percentile_cont(0.01) within group (order by l.labresult) as p01,
  percentile_cont(0.5) within group (order by l.labresult) as median,
  percentile_cont(0.99) within group (order by l.labresult) as p99,
  max(l.labresult) as max_value
from eicu_crd.lab l
join cohort c using (patientunitstayid)
where l.labresult is not null
  and l.labresultoffset between -360 and 1440
  and lower(l.labname) in ('creatinine','total bilirubin','lactate','platelets x 1000','wbc x 1000','pao2','fio2')
group by l.labname, coalesce(l.labmeasurenamesystem, '')
order by l.labname, stays_m6_24 desc
")

vital_sources_sql <- paste0(base_cte, feature_cte, "
, long_values as (
  select 'apacheapsvar_meanbp'::text as variable, meanbp_apache::numeric as value from feature where meanbp_apache is not null
  union all select 'vitalperiodic_systemicmean_min_0_24', meanbp_min_vitalperiodic_0_24::numeric from feature where meanbp_min_vitalperiodic_0_24 is not null
  union all select 'vitalaperiodic_noninvasivemean_min_0_24', meanbp_min_vitalaperiodic_0_24::numeric from feature where meanbp_min_vitalaperiodic_0_24 is not null
  union all select 'apacheapsvar_heartrate', heartrate_apache::numeric from feature where heartrate_apache is not null
  union all select 'vitalperiodic_heartrate_max_0_24', heartrate_max_vitalperiodic_0_24::numeric from feature where heartrate_max_vitalperiodic_0_24 is not null
  union all select 'apacheapsvar_respiratoryrate', respiratoryrate_apache::numeric from feature where respiratoryrate_apache is not null
  union all select 'vitalperiodic_respiration_max_0_24', respiration_max_vitalperiodic_0_24::numeric from feature where respiration_max_vitalperiodic_0_24 is not null
)
select
  variable,
  count(*) as stays_n,
  round((100.0 * count(*) / nullif((select count(*) from feature), 0))::numeric, 2) as coverage_pct,
  min(value) as min_value,
  percentile_cont(0.01) within group (order by value) as p01,
  percentile_cont(0.5) within group (order by value) as median,
  percentile_cont(0.99) within group (order by value) as p99,
  max(value) as max_value
from long_values
group by variable
order by variable
")

vasopressor_terms_sql <- sprintf("%s
select
  i.drugname,
  count(*) as rows_0_24,
  count(distinct i.patientunitstayid) as stays_0_24
from eicu_crd.infusiondrug i
join cohort c using (patientunitstayid)
where i.infusionoffset between 0 and 1440
  and i.drugname ~* %s
group by i.drugname
order by rows_0_24 desc, i.drugname
limit 100
",
base_cte,
sql_literal(vasopressor_regex)
)

print_section("Run covariate coverage summary")
coverage <- run_query(coverage_sql)
setDT(coverage)

print_section("Run numeric value diagnostics")
value_diag <- run_query(value_diag_sql)
setDT(value_diag)

print_section("Run APACHE record diagnostics")
record_diag <- run_query(record_diag_sql)
setDT(record_diag)

print_section("Run lab unit diagnostics")
lab_units <- run_query(lab_units_sql)
setDT(lab_units)

print_section("Run vital source diagnostics")
vital_sources <- run_query(vital_sources_sql)
setDT(vital_sources)

print_section("Run vasopressor term diagnostics")
vasopressor_terms <- run_query(vasopressor_terms_sql)
setDT(vasopressor_terms)

fwrite(coverage, out_coverage)
fwrite(value_diag, out_value_diag)
fwrite(record_diag, out_record_diag)
fwrite(lab_units, out_lab_units)
fwrite(vital_sources, out_vital_sources)
fwrite(vasopressor_terms, out_vasopressor_terms)

coverage_key <- coverage[
  variable %in% c(
    "age_num", "gender", "ethnicity", "aps_iva", "apache_score_iva",
    "vent_apache", "meanbp_apache", "creatinine_apache", "bilirubin_apache",
    "urine_apache", "lab_lactate_0_24", "vasopressor_rows_0_24",
    "diabetes_pred", "immunosuppression_pred", "meanbp_min_vitalperiodic_0_24"
  )
]

report_lines <- c(
  "eICU covariate coverage audit",
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Database: ", pg_db, " / schema eicu_crd"),
  paste0("Base directory: ", base_dir),
  "",
  "Cohort:",
  "- strict explicit sepsis label",
  "- adult",
  "- first qualifying stay per uniquepid",
  "- ICU-admission offset 0 t0",
  "- antibiotics exclude non-administration terms and PO/local route terms",
  "",
  "Key coverage rows:",
  paste(capture.output(print(coverage_key)), collapse = "\n"),
  "",
  "APACHE record diagnostics:",
  paste(capture.output(print(record_diag)), collapse = "\n"),
  "",
  "Lab unit diagnostics:",
  paste(capture.output(print(lab_units)), collapse = "\n"),
  "",
  "Vital source diagnostics:",
  paste(capture.output(print(vital_sources)), collapse = "\n"),
  "",
  "Top vasopressor terms:",
  paste(capture.output(print(head(vasopressor_terms, 20))), collapse = "\n"),
  "",
  "Notes:",
  "- This is a coverage audit only; no imputation, no final cleaning, and no model table are produced.",
  "- APACHE-derived variables are high-coverage candidates for the first eICU Table 1/model.",
  "- Raw lab/vital sources are audited for rescue or sensitivity use and require range review before final merging.",
  "- Vasopressor absence is not automatically treated as no shock in this audit; final shock proxy needs a rule decision."
)

writeLines(report_lines, out_report, useBytes = TRUE)

print_section("Done")
cat("Wrote:\n")
cat(" - ", out_coverage, "\n", sep = "")
cat(" - ", out_value_diag, "\n", sep = "")
cat(" - ", out_record_diag, "\n", sep = "")
cat(" - ", out_lab_units, "\n", sep = "")
cat(" - ", out_vital_sources, "\n", sep = "")
cat(" - ", out_vasopressor_terms, "\n", sep = "")
cat(" - ", out_report, "\n", sep = "")

