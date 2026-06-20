/* eICU formal_v1 full wide-table build SQL, 2026-06-13

Study position:
  ICU-admission-anchored directional external validation.

Anchor:
  t0 = ICU admission offset 0.

Outcome:
  30-day in-hospital mortality.

Main exposure:
  arm2 = 0-3h vs 3-12h antibiotic initiation after ICU admission.

Main outputs:
  public.eicu_formal_v1_analysis_main
  public.eicu_formal_v1_analysis_main_apache_raw
  public.eicu_formal_v1_analysis_main_apache_clean
  public.eicu_formal_v1_analysis_main_model_ready_wide
  public.eicu_formal_v1_arm2_model_dataset

Run order:
  Run this file from top to bottom in Navicat/PostgreSQL.
*/


/* -------------------------------------------------------------------------
   Optional speed indexes on source tables.
   These do not change data values.
------------------------------------------------------------------------- */

CREATE INDEX IF NOT EXISTS idx_eicu_diagnosis_stay
ON eicu_crd.diagnosis (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_admissiondx_stay
ON eicu_crd.admissiondx (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_medication_stay
ON eicu_crd.medication (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_infusiondrug_stay
ON eicu_crd.infusiondrug (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_apachepatientresult_stay
ON eicu_crd.apachepatientresult (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_apacheapsvar_stay
ON eicu_crd.apacheapsvar (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_apachepredvar_stay
ON eicu_crd.apachepredvar (patientunitstayid);


/* -------------------------------------------------------------------------
   Step 01. First observed ICU stay per uniquepid.

   Expected current result:
     rows_n = 139367
     adult_n = 138855
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_first_icu_stay;

CREATE TABLE public.eicu_formal_v1_first_icu_stay AS
WITH patient_base AS (
  SELECT
    p.patientunitstayid,
    p.patienthealthsystemstayid,
    p.uniquepid,
    p.hospitalid,
    p.wardid,
    p.gender,
    p.age AS age_raw,
    CASE
      WHEN p.age ~ '^[0-9]+$' THEN p.age::int
      WHEN p.age = '> 89' THEN 90
      ELSE NULL
    END AS age_num,
    p.ethnicity,
    p.hospitaladmitsource,
    p.unitadmitsource,
    p.unittype,
    p.unitstaytype,
    p.unitvisitnumber,
    p.apacheadmissiondx,
    p.hospitaladmitoffset,
    p.admissionheight,
    p.admissionweight,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,
    p.unitdischargeoffset,
    p.unitdischargestatus
  FROM eicu_crd.patient p
),
ranked AS (
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY COALESCE(NULLIF(uniquepid, ''), patientunitstayid::text)
      ORDER BY
        COALESCE(unitvisitnumber, 999999),
        patientunitstayid
    ) AS rn_first_icu
  FROM patient_base
)
SELECT *
FROM ranked
WHERE rn_first_icu = 1;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_first_icu_stay_id
ON public.eicu_formal_v1_first_icu_stay (patientunitstayid);

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  SUM((age_num >= 18)::int) AS adult_n,
  SUM((age_num IS NULL)::int) AS age_missing_n
FROM public.eicu_formal_v1_first_icu_stay;

SELECT
  unitvisitnumber,
  COUNT(*) AS n
FROM public.eicu_formal_v1_first_icu_stay
GROUP BY unitvisitnumber
ORDER BY unitvisitnumber;


/* -------------------------------------------------------------------------
   Step 02. Adult first observed ICU stay.

   Expected current result:
     rows_n = 138855
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_adult_first_icu_stay;

CREATE TABLE public.eicu_formal_v1_adult_first_icu_stay AS
SELECT *
FROM public.eicu_formal_v1_first_icu_stay
WHERE age_num >= 18;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_adult_first_icu_stay_id
ON public.eicu_formal_v1_adult_first_icu_stay (patientunitstayid);

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  MIN(age_num) AS min_age,
  MAX(age_num) AS max_age
FROM public.eicu_formal_v1_adult_first_icu_stay;


/* -------------------------------------------------------------------------
   Step 03. Strict explicit sepsis labels within adult first ICU stays.
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_adult_first_icu_sepsis_label;

CREATE TABLE public.eicu_formal_v1_adult_first_icu_sepsis_label AS
WITH sepsis_label_raw AS (
  SELECT
    d.patientunitstayid,
    d.diagnosisoffset AS label_offset_min,
    'diagnosis'::text AS label_source,
    d.diagnosisstring::text AS label_text
  FROM public.eicu_formal_v1_adult_first_icu_stay b
  JOIN eicu_crd.diagnosis d
    ON d.patientunitstayid = b.patientunitstayid
  WHERE d.diagnosisstring ~* '\m(sepsis|septic|septicemia|urosepsis)\M'

  UNION ALL

  SELECT
    a.patientunitstayid,
    a.admitdxenteredoffset AS label_offset_min,
    'admissiondx'::text AS label_source,
    concat_ws(' | ', a.admitdxpath, a.admitdxname, a.admitdxtext)::text AS label_text
  FROM public.eicu_formal_v1_adult_first_icu_stay b
  JOIN eicu_crd.admissiondx a
    ON a.patientunitstayid = b.patientunitstayid
  WHERE a.admitdxpath ~* '\m(sepsis|septic|septicemia|urosepsis)\M'
     OR coalesce(a.admitdxname, '') ~* '\m(sepsis|septic|septicemia|urosepsis)\M'
     OR coalesce(a.admitdxtext, '') ~* '\m(sepsis|septic|septicemia|urosepsis)\M'
)
SELECT
  patientunitstayid,
  MIN(label_offset_min) AS sepsis_label_first_offset_min,
  COUNT(*) AS sepsis_label_rows,
  BOOL_OR(label_source = 'diagnosis') AS has_diagnosis_sepsis,
  BOOL_OR(label_source = 'admissiondx') AS has_admissiondx_sepsis,
  BOOL_OR(label_text ~* '\m(sepsis|septic|septicemia|urosepsis)\M') AS has_broad_sepsis_label,
  BOOL_OR(
    label_text ~* '\m(sepsis|septic|septicemia|urosepsis)\M'
    AND label_text !~* '(signs and symptoms of sepsis|\mSIRS\M)'
  ) AS has_strict_explicit_sepsis
FROM sepsis_label_raw
GROUP BY patientunitstayid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_sepsis_label_id
ON public.eicu_formal_v1_adult_first_icu_sepsis_label (patientunitstayid);


/* -------------------------------------------------------------------------
   Step 04. Strict explicit sepsis adult first ICU cohort.

   Expected current result:
     rows_n = 21326
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_strict_sepsis_adult_first_icu;

CREATE TABLE public.eicu_formal_v1_strict_sepsis_adult_first_icu AS
SELECT
  b.*,
  s.sepsis_label_first_offset_min,
  s.sepsis_label_rows,
  s.has_diagnosis_sepsis,
  s.has_admissiondx_sepsis,
  s.has_broad_sepsis_label,
  s.has_strict_explicit_sepsis
FROM public.eicu_formal_v1_adult_first_icu_stay b
JOIN public.eicu_formal_v1_adult_first_icu_sepsis_label s
  ON s.patientunitstayid = b.patientunitstayid
WHERE s.has_strict_explicit_sepsis = TRUE;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_strict_sepsis_id
ON public.eicu_formal_v1_strict_sepsis_adult_first_icu (patientunitstayid);

SELECT
  COUNT(*) AS strict_sepsis_first_icu_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  SUM((has_diagnosis_sepsis)::int) AS has_diagnosis_sepsis_n,
  SUM((has_admissiondx_sepsis)::int) AS has_admissiondx_sepsis_n
FROM public.eicu_formal_v1_strict_sepsis_adult_first_icu;


/* -------------------------------------------------------------------------
   Step 05. Antibiotic candidate records.

   Notes:
     - t0 is ICU admission offset 0.
     - Medication and infusiondrug are used as timing sources.
     - treatment table is not used for timing because it is care-plan
       documentation rather than actual administration time.
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_strict_sepsis_abx_rows;

CREATE TABLE public.eicu_formal_v1_strict_sepsis_abx_rows AS
WITH abx_raw AS (
  SELECT
    m.patientunitstayid,
    m.drugstartoffset AS abx_offset_min,
    m.drugname::text AS drugname,
    'medication'::text AS abx_source
  FROM public.eicu_formal_v1_strict_sepsis_adult_first_icu c
  JOIN eicu_crd.medication m
    ON m.patientunitstayid = c.patientunitstayid
  WHERE m.drugname ~* '\m(vancomycin|vancocin|vanco|cefepime|ceftriaxone|ceftazidime|cefazolin|piperacillin|tazobactam|zosyn|meropenem|imipenem|ertapenem|azithromycin|levofloxacin|levaquin|ciprofloxacin|moxifloxacin|ampicillin|unasyn|gentamicin|tobramycin|amikacin|metronidazole|flagyl|linezolid|daptomycin|clindamycin|doxycycline)\M'
    AND m.drugname !~* '\m(consult|pharmacy|trough|level|random|peak|placeholder|protocol|monitor)\M'
    AND m.drugname !~* '\m(PO|ORAL|TAB|TABS|TABLET|TABLETS|CAP|CAPS|CAPSULE|CAPSULES|SUSP|SUSPENSION|ELIXIR|OPHTH|OPHTHALMIC|OTIC|TOPICAL|CREAM|OINT|OINTMENT|NEB|NEBULIZER|INHAL|INHALATION|MOUTH|IRRIG|IRRIGATION|LOCK)\M'
    AND COALESCE(m.drugordercancelled, '') !~* 'yes|true|cancel'
    AND m.drugstartoffset IS NOT NULL

  UNION ALL

  SELECT
    i.patientunitstayid,
    i.infusionoffset AS abx_offset_min,
    i.drugname::text AS drugname,
    'infusiondrug'::text AS abx_source
  FROM public.eicu_formal_v1_strict_sepsis_adult_first_icu c
  JOIN eicu_crd.infusiondrug i
    ON i.patientunitstayid = c.patientunitstayid
  WHERE i.drugname ~* '\m(vancomycin|vancocin|vanco|cefepime|ceftriaxone|ceftazidime|cefazolin|piperacillin|tazobactam|zosyn|meropenem|imipenem|ertapenem|azithromycin|levofloxacin|levaquin|ciprofloxacin|moxifloxacin|ampicillin|unasyn|gentamicin|tobramycin|amikacin|metronidazole|flagyl|linezolid|daptomycin|clindamycin|doxycycline)\M'
    AND i.drugname !~* '\m(consult|pharmacy|trough|level|random|peak|placeholder|protocol|monitor)\M'
    AND i.drugname !~* '\m(PO|ORAL|TAB|TABS|TABLET|TABLETS|CAP|CAPS|CAPSULE|CAPSULES|SUSP|SUSPENSION|ELIXIR|OPHTH|OPHTHALMIC|OTIC|TOPICAL|CREAM|OINT|OINTMENT|NEB|NEBULIZER|INHAL|INHALATION|MOUTH|IRRIG|IRRIGATION|LOCK)\M'
    AND i.infusionoffset IS NOT NULL
)
SELECT *
FROM abx_raw;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_abx_rows_id
ON public.eicu_formal_v1_strict_sepsis_abx_rows (patientunitstayid);


/* -------------------------------------------------------------------------
   Step 06. First antibiotic record per stay.

   Expected current distribution:
     pre_t0_abx    = 7038
     0-3h          = 2074
     3-6h          = 696
     6-12h         = 491
     >12-72h       = 1035
     >72h          = 321
     no_abx_record = 9671
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_strict_sepsis_first_abx;

CREATE TABLE public.eicu_formal_v1_strict_sepsis_first_abx AS
SELECT DISTINCT ON (patientunitstayid)
  patientunitstayid,
  abx_offset_min AS first_abx_offset_min,
  ROUND((abx_offset_min::numeric / 60.0), 3) AS first_abx_delay_hours_from_unit,
  drugname AS first_abx_drugname,
  abx_source AS first_abx_source
FROM public.eicu_formal_v1_strict_sepsis_abx_rows
ORDER BY patientunitstayid, abx_offset_min, abx_source, drugname;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_first_abx_id
ON public.eicu_formal_v1_strict_sepsis_first_abx (patientunitstayid);

WITH windowed AS (
  SELECT
    CASE
      WHEN f.first_abx_offset_min IS NULL THEN 'no_abx_record'
      WHEN f.first_abx_offset_min < 0 THEN 'pre_t0_abx'
      WHEN f.first_abx_offset_min <= 180 THEN '0-3h'
      WHEN f.first_abx_offset_min <= 360 THEN '3-6h'
      WHEN f.first_abx_offset_min <= 720 THEN '6-12h'
      WHEN f.first_abx_offset_min <= 4320 THEN '>12-72h'
      ELSE '>72h'
    END AS abx_window_unit_t0
  FROM public.eicu_formal_v1_strict_sepsis_adult_first_icu c
  LEFT JOIN public.eicu_formal_v1_strict_sepsis_first_abx f
    ON f.patientunitstayid = c.patientunitstayid
)
SELECT
  abx_window_unit_t0,
  COUNT(*) AS n
FROM windowed
GROUP BY abx_window_unit_t0
ORDER BY
  CASE abx_window_unit_t0
    WHEN 'pre_t0_abx' THEN 1
    WHEN '0-3h' THEN 2
    WHEN '3-6h' THEN 3
    WHEN '6-12h' THEN 4
    WHEN '>12-72h' THEN 5
    WHEN '>72h' THEN 6
    WHEN 'no_abx_record' THEN 7
    ELSE 99
  END;


/* -------------------------------------------------------------------------
   Step 07. Exposure-outcome backbone wide table.

   Output:
     public.eicu_formal_v1_analysis_main

   Expected current result:
     rows_n = 21326
     death_30d_inhosp_n = 4027 on full denominator
     outcome missing = 235
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_analysis_main;

CREATE TABLE public.eicu_formal_v1_analysis_main AS
SELECT
  c.*,

  f.first_abx_offset_min,
  f.first_abx_delay_hours_from_unit,
  f.first_abx_drugname,
  f.first_abx_source,

  CASE
    WHEN f.first_abx_offset_min IS NULL THEN 'no_abx_record'
    WHEN f.first_abx_offset_min < 0 THEN 'pre_t0_abx'
    WHEN f.first_abx_offset_min <= 180 THEN '0-3h'
    WHEN f.first_abx_offset_min <= 360 THEN '3-6h'
    WHEN f.first_abx_offset_min <= 720 THEN '6-12h'
    WHEN f.first_abx_offset_min <= 4320 THEN '>12-72h'
    ELSE '>72h'
  END AS abx_window_unit_t0,

  CASE
    WHEN f.first_abx_offset_min IS NULL THEN 0
    WHEN f.first_abx_offset_min BETWEEN 0 AND 4320 THEN 1
    ELSE 0
  END AS post_t0_abx_0_72h,

  CASE
    WHEN f.first_abx_offset_min IS NULL THEN 0
    WHEN f.first_abx_offset_min BETWEEN 0 AND 720 THEN 1
    ELSE 0
  END AS main_three_window_candidate,

  CASE WHEN f.first_abx_offset_min < 0 THEN 1 ELSE 0 END AS pre_t0_abx,
  CASE WHEN f.first_abx_offset_min IS NULL THEN 1 ELSE 0 END AS no_abx_record,

  CASE
    WHEN lower(coalesce(c.hospitaldischargestatus, '')) = 'expired'
         AND c.hospitaldischargeoffset <= 43200
      THEN 1
    WHEN c.hospitaldischargestatus IS NULL OR c.hospitaldischargestatus = ''
      THEN NULL
    ELSE 0
  END AS death_30d_inhosp,

  CASE
    WHEN lower(coalesce(c.hospitaldischargestatus, '')) = 'expired'
      THEN 1
    WHEN c.hospitaldischargestatus IS NULL OR c.hospitaldischargestatus = ''
      THEN NULL
    ELSE 0
  END AS death_hosp,

  CASE
    WHEN lower(coalesce(c.unitdischargestatus, '')) = 'expired'
      THEN 1
    WHEN c.unitdischargestatus IS NULL OR c.unitdischargestatus = ''
      THEN NULL
    ELSE 0
  END AS death_icu,

  CASE
    WHEN c.hospitaldischargeoffset IS NOT NULL
         AND c.hospitaldischargeoffset >= 0
      THEN LEAST(c.hospitaldischargeoffset, 43200)::numeric / 1440.0
    ELSE NULL
  END AS followup_30d_inhosp_days,

  0 AS t0_offset_min,
  'ICU admission offset 0'::text AS t0_definition

FROM public.eicu_formal_v1_strict_sepsis_adult_first_icu c
LEFT JOIN public.eicu_formal_v1_strict_sepsis_first_abx f
  ON f.patientunitstayid = c.patientunitstayid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_analysis_main_id
ON public.eicu_formal_v1_analysis_main (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_analysis_main_hosp
ON public.eicu_formal_v1_analysis_main (hospitalid);

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  SUM(death_30d_inhosp) AS death_30d_inhosp_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / COUNT(*), 2) AS death_30d_inhosp_pct_full_denom,
  SUM((death_30d_inhosp IS NULL)::int) AS outcome_missing_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / SUM((death_30d_inhosp IS NOT NULL)::int), 2) AS death_30d_inhosp_pct_nonmissing_denom,
  SUM(pre_t0_abx) AS pre_t0_abx_n,
  SUM(no_abx_record) AS no_abx_record_n,
  SUM(main_three_window_candidate) AS main_three_window_candidate_n,
  SUM(post_t0_abx_0_72h) AS post_t0_abx_0_72h_n
FROM public.eicu_formal_v1_analysis_main;

SELECT
  abx_window_unit_t0,
  COUNT(*) AS n,
  SUM(death_30d_inhosp) AS death_30d_inhosp_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / SUM((death_30d_inhosp IS NOT NULL)::int), 2) AS death_30d_inhosp_pct_nonmissing_denom
FROM public.eicu_formal_v1_analysis_main
GROUP BY abx_window_unit_t0
ORDER BY
  CASE abx_window_unit_t0
    WHEN 'pre_t0_abx' THEN 1
    WHEN '0-3h' THEN 2
    WHEN '3-6h' THEN 3
    WHEN '6-12h' THEN 4
    WHEN '>12-72h' THEN 5
    WHEN '>72h' THEN 6
    WHEN 'no_abx_record' THEN 7
    ELSE 99
  END;


/* -------------------------------------------------------------------------
   Step 08. APACHE raw merge.

   Version rule:
     Use APACHE IVa from apachepatientresult.

   Expected current coverage:
     apachepatientresult IVa = 18448 stays
     apacheapsvar = 20904 stays
     apachepredvar = 20904 stays
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_apachepatientresult_iva_one;

CREATE TABLE public.eicu_formal_v1_apachepatientresult_iva_one AS
SELECT DISTINCT ON (r.patientunitstayid)
  r.apachepatientresultsid AS apachepatientresultsid_iva,
  r.patientunitstayid,
  r.physicianspeciality,
  r.physicianinterventioncategory,
  r.acutephysiologyscore AS aps_iva_raw,
  r.apachescore AS apache_score_iva_raw,
  r.apacheversion AS apacheversion_iva,
  r.predictedicumortality AS pred_icu_mort_iva_raw,
  r.actualicumortality AS actual_icu_mort_iva_raw,
  r.predictediculos AS predicted_icu_los_iva_raw,
  r.actualiculos AS actual_icu_los_iva_raw,
  r.predictedhospitalmortality AS pred_hosp_mort_iva_raw,
  r.actualhospitalmortality AS actual_hosp_mort_iva_raw,
  r.predictedhospitallos AS predicted_hosp_los_iva_raw,
  r.actualhospitallos AS actual_hosp_los_iva_raw,
  r.actualventdays AS actual_vent_days_iva_raw,
  r.predventdays AS pred_vent_days_iva_raw
FROM eicu_crd.apachepatientresult r
JOIN public.eicu_formal_v1_analysis_main m
  ON m.patientunitstayid = r.patientunitstayid
WHERE r.apacheversion = 'IVa'
ORDER BY
  r.patientunitstayid,
  r.apachepatientresultsid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_apr_iva_one_id
ON public.eicu_formal_v1_apachepatientresult_iva_one (patientunitstayid);

DROP TABLE IF EXISTS public.eicu_formal_v1_apacheapsvar_one;

CREATE TABLE public.eicu_formal_v1_apacheapsvar_one AS
SELECT DISTINCT ON (a.patientunitstayid)
  a.*
FROM eicu_crd.apacheapsvar a
JOIN public.eicu_formal_v1_analysis_main m
  ON m.patientunitstayid = a.patientunitstayid
ORDER BY
  a.patientunitstayid,
  a.apacheapsvarid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_apacheapsvar_one_id
ON public.eicu_formal_v1_apacheapsvar_one (patientunitstayid);

DROP TABLE IF EXISTS public.eicu_formal_v1_apachepredvar_one;

CREATE TABLE public.eicu_formal_v1_apachepredvar_one AS
SELECT DISTINCT ON (p.patientunitstayid)
  p.*
FROM eicu_crd.apachepredvar p
JOIN public.eicu_formal_v1_analysis_main m
  ON m.patientunitstayid = p.patientunitstayid
ORDER BY
  p.patientunitstayid,
  p.apachepredvarid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_apachepredvar_one_id
ON public.eicu_formal_v1_apachepredvar_one (patientunitstayid);

SELECT 'apachepatientresult_iva_one' AS table_name, COUNT(*) AS rows_n, COUNT(DISTINCT patientunitstayid) AS distinct_stays
FROM public.eicu_formal_v1_apachepatientresult_iva_one
UNION ALL
SELECT 'apacheapsvar_one' AS table_name, COUNT(*) AS rows_n, COUNT(DISTINCT patientunitstayid) AS distinct_stays
FROM public.eicu_formal_v1_apacheapsvar_one
UNION ALL
SELECT 'apachepredvar_one' AS table_name, COUNT(*) AS rows_n, COUNT(DISTINCT patientunitstayid) AS distinct_stays
FROM public.eicu_formal_v1_apachepredvar_one;

DROP TABLE IF EXISTS public.eicu_formal_v1_analysis_main_apache_raw;

CREATE TABLE public.eicu_formal_v1_analysis_main_apache_raw AS
SELECT
  m.*,

  apr.apachepatientresultsid_iva,
  apr.physicianspeciality,
  apr.physicianinterventioncategory,
  apr.aps_iva_raw,
  apr.apache_score_iva_raw,
  apr.apacheversion_iva,
  apr.pred_icu_mort_iva_raw,
  apr.actual_icu_mort_iva_raw,
  apr.predicted_icu_los_iva_raw,
  apr.actual_icu_los_iva_raw,
  apr.pred_hosp_mort_iva_raw,
  apr.actual_hosp_mort_iva_raw,
  apr.predicted_hosp_los_iva_raw,
  apr.actual_hosp_los_iva_raw,
  apr.actual_vent_days_iva_raw,
  apr.pred_vent_days_iva_raw,

  a.apacheapsvarid,
  a.intubated AS intubated_apache_raw,
  a.vent AS vent_apache_raw,
  a.dialysis AS dialysis_apache_raw,
  a.eyes AS eyes_apache_raw,
  a.motor AS motor_apache_raw,
  a.verbal AS verbal_apache_raw,
  a.meds AS meds_apache_raw,
  a.urine AS urine_apache_raw,
  a.wbc AS wbc_apache_raw,
  a.temperature AS temperature_apache_raw,
  a.respiratoryrate AS respiratoryrate_apache_raw,
  a.sodium AS sodium_apache_raw,
  a.heartrate AS heartrate_apache_raw,
  a.meanbp AS meanbp_apache_raw,
  a.ph AS ph_apache_raw,
  a.hematocrit AS hematocrit_apache_raw,
  a.creatinine AS creatinine_apache_raw,
  a.albumin AS albumin_apache_raw,
  a.pao2 AS pao2_apache_raw,
  a.pco2 AS pco2_apache_raw,
  a.bun AS bun_apache_raw,
  a.glucose AS glucose_apache_raw,
  a.bilirubin AS bilirubin_apache_raw,
  a.fio2 AS fio2_apache_raw,

  p.apachepredvarid,
  p.admitdiagnosis AS admitdiagnosis_pred_raw,
  p.aids AS aids_pred_raw,
  p.hepaticfailure AS hepaticfailure_pred_raw,
  p.lymphoma AS lymphoma_pred_raw,
  p.metastaticcancer AS metastaticcancer_pred_raw,
  p.leukemia AS leukemia_pred_raw,
  p.immunosuppression AS immunosuppression_pred_raw,
  p.cirrhosis AS cirrhosis_pred_raw,
  p.electivesurgery AS electivesurgery_pred_raw,
  p.activetx AS activetx_pred_raw,
  p.readmit AS readmit_pred_raw,
  p.ventday1 AS ventday1_pred_raw,
  p.oobventday1 AS oobventday1_pred_raw,
  p.oobintubday1 AS oobintubday1_pred_raw,
  p.diabetes AS diabetes_pred_raw

FROM public.eicu_formal_v1_analysis_main m
LEFT JOIN public.eicu_formal_v1_apachepatientresult_iva_one apr
  ON apr.patientunitstayid = m.patientunitstayid
LEFT JOIN public.eicu_formal_v1_apacheapsvar_one a
  ON a.patientunitstayid = m.patientunitstayid
LEFT JOIN public.eicu_formal_v1_apachepredvar_one p
  ON p.patientunitstayid = m.patientunitstayid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_analysis_main_apache_raw_id
ON public.eicu_formal_v1_analysis_main_apache_raw (patientunitstayid);

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  SUM((apachepatientresultsid_iva IS NOT NULL)::int) AS has_apache_iva_n,
  SUM((apacheapsvarid IS NOT NULL)::int) AS has_apacheapsvar_n,
  SUM((apachepredvarid IS NOT NULL)::int) AS has_apachepredvar_n
FROM public.eicu_formal_v1_analysis_main_apache_raw;

DO $$
DECLARE
  v_raw_rows integer;
  v_raw_stays integer;
  v_cohort_rows integer;
BEGIN
  SELECT COUNT(*), COUNT(DISTINCT patientunitstayid)
  INTO v_raw_rows, v_raw_stays
  FROM public.eicu_formal_v1_analysis_main_apache_raw;

  SELECT COUNT(*)
  INTO v_cohort_rows
  FROM public.eicu_formal_v1_analysis_main;

  IF v_raw_rows <> v_cohort_rows OR v_raw_stays <> v_cohort_rows THEN
    RAISE EXCEPTION
      'APACHE raw merge row-count check failed: rows=%, distinct_stays=%, expected=%',
      v_raw_rows, v_raw_stays, v_cohort_rows;
  END IF;
END;
$$;


/* -------------------------------------------------------------------------
   Step 09. APACHE clean table.

   Rule:
     eICU APACHE numeric sentinel -1 is missing, not a physiologic value.

   Output:
     public.eicu_formal_v1_analysis_main_apache_clean
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_analysis_main_apache_clean;

CREATE TABLE public.eicu_formal_v1_analysis_main_apache_clean AS
SELECT
  r.*,

  CASE WHEN r.apachepatientresultsid_iva IS NULL THEN 1 ELSE 0 END AS apache_iva_missing_ind,
  CASE WHEN r.apacheapsvarid IS NULL THEN 1 ELSE 0 END AS apacheapsvar_missing_ind,
  CASE WHEN r.apachepredvarid IS NULL THEN 1 ELSE 0 END AS apachepredvar_missing_ind,

  CASE WHEN r.aps_iva_raw = -1 THEN 1 ELSE 0 END AS aps_iva_sentinel_ind,
  CASE WHEN r.aps_iva_raw IS NULL OR r.aps_iva_raw = -1 THEN 1 ELSE 0 END AS aps_iva_missing_ind,
  CASE WHEN r.aps_iva_raw IS NULL OR r.aps_iva_raw = -1 THEN NULL ELSE r.aps_iva_raw END AS aps_iva_clean,

  CASE WHEN r.apache_score_iva_raw = -1 THEN 1 ELSE 0 END AS apache_score_iva_sentinel_ind,
  CASE WHEN r.apache_score_iva_raw IS NULL OR r.apache_score_iva_raw = -1 THEN 1 ELSE 0 END AS apache_score_iva_missing_ind,
  CASE WHEN r.apache_score_iva_raw IS NULL OR r.apache_score_iva_raw = -1 THEN NULL ELSE r.apache_score_iva_raw END AS apache_score_iva_clean,

  CASE WHEN trim(coalesce(r.pred_hosp_mort_iva_raw, '')) IN ('-1', '-1.0') THEN 1 ELSE 0 END AS pred_hosp_mort_iva_sentinel_ind,
  CASE
    WHEN r.pred_hosp_mort_iva_raw IS NULL THEN 1
    WHEN trim(r.pred_hosp_mort_iva_raw) IN ('-1', '-1.0') THEN 1
    WHEN trim(r.pred_hosp_mort_iva_raw) !~ '^-?[0-9]+([.][0-9]+)?$' THEN 1
    ELSE 0
  END AS pred_hosp_mort_iva_missing_ind,
  CASE
    WHEN r.pred_hosp_mort_iva_raw IS NULL THEN NULL
    WHEN trim(r.pred_hosp_mort_iva_raw) IN ('-1', '-1.0') THEN NULL
    WHEN trim(r.pred_hosp_mort_iva_raw) ~ '^-?[0-9]+([.][0-9]+)?$' THEN trim(r.pred_hosp_mort_iva_raw)::double precision
    ELSE NULL
  END AS pred_hosp_mort_iva_clean,

  CASE WHEN trim(coalesce(r.pred_icu_mort_iva_raw, '')) IN ('-1', '-1.0') THEN 1 ELSE 0 END AS pred_icu_mort_iva_sentinel_ind,
  CASE
    WHEN r.pred_icu_mort_iva_raw IS NULL THEN 1
    WHEN trim(r.pred_icu_mort_iva_raw) IN ('-1', '-1.0') THEN 1
    WHEN trim(r.pred_icu_mort_iva_raw) !~ '^-?[0-9]+([.][0-9]+)?$' THEN 1
    ELSE 0
  END AS pred_icu_mort_iva_missing_ind,
  CASE
    WHEN r.pred_icu_mort_iva_raw IS NULL THEN NULL
    WHEN trim(r.pred_icu_mort_iva_raw) IN ('-1', '-1.0') THEN NULL
    WHEN trim(r.pred_icu_mort_iva_raw) ~ '^-?[0-9]+([.][0-9]+)?$' THEN trim(r.pred_icu_mort_iva_raw)::double precision
    ELSE NULL
  END AS pred_icu_mort_iva_clean,

  CASE WHEN r.intubated_apache_raw = -1 THEN 1 ELSE 0 END AS intubated_apache_sentinel_ind,
  CASE WHEN r.intubated_apache_raw IS NULL OR r.intubated_apache_raw = -1 THEN 1 ELSE 0 END AS intubated_apache_missing_ind,
  CASE WHEN r.intubated_apache_raw IS NULL OR r.intubated_apache_raw = -1 THEN NULL ELSE r.intubated_apache_raw END AS intubated_apache_clean,

  CASE WHEN r.vent_apache_raw = -1 THEN 1 ELSE 0 END AS vent_apache_sentinel_ind,
  CASE WHEN r.vent_apache_raw IS NULL OR r.vent_apache_raw = -1 THEN 1 ELSE 0 END AS vent_apache_missing_ind,
  CASE WHEN r.vent_apache_raw IS NULL OR r.vent_apache_raw = -1 THEN NULL ELSE r.vent_apache_raw END AS vent_apache_clean,

  CASE WHEN r.dialysis_apache_raw = -1 THEN 1 ELSE 0 END AS dialysis_apache_sentinel_ind,
  CASE WHEN r.dialysis_apache_raw IS NULL OR r.dialysis_apache_raw = -1 THEN 1 ELSE 0 END AS dialysis_apache_missing_ind,
  CASE WHEN r.dialysis_apache_raw IS NULL OR r.dialysis_apache_raw = -1 THEN NULL ELSE r.dialysis_apache_raw END AS dialysis_apache_clean,

  CASE WHEN r.heartrate_apache_raw = -1 THEN 1 ELSE 0 END AS heartrate_apache_sentinel_ind,
  CASE WHEN r.heartrate_apache_raw IS NULL OR r.heartrate_apache_raw = -1 THEN 1 ELSE 0 END AS heartrate_apache_missing_ind,
  CASE WHEN r.heartrate_apache_raw IS NULL OR r.heartrate_apache_raw = -1 THEN NULL ELSE r.heartrate_apache_raw END AS heartrate_apache_clean,

  CASE WHEN r.meanbp_apache_raw = -1 THEN 1 ELSE 0 END AS meanbp_apache_sentinel_ind,
  CASE WHEN r.meanbp_apache_raw IS NULL OR r.meanbp_apache_raw = -1 THEN 1 ELSE 0 END AS meanbp_apache_missing_ind,
  CASE WHEN r.meanbp_apache_raw IS NULL OR r.meanbp_apache_raw = -1 THEN NULL ELSE r.meanbp_apache_raw END AS meanbp_apache_clean,

  CASE WHEN r.respiratoryrate_apache_raw = -1 THEN 1 ELSE 0 END AS respiratoryrate_apache_sentinel_ind,
  CASE WHEN r.respiratoryrate_apache_raw IS NULL OR r.respiratoryrate_apache_raw = -1 THEN 1 ELSE 0 END AS respiratoryrate_apache_missing_ind,
  CASE WHEN r.respiratoryrate_apache_raw IS NULL OR r.respiratoryrate_apache_raw = -1 THEN NULL ELSE r.respiratoryrate_apache_raw END AS respiratoryrate_apache_clean,

  CASE WHEN r.temperature_apache_raw = -1 THEN 1 ELSE 0 END AS temperature_apache_sentinel_ind,
  CASE WHEN r.temperature_apache_raw IS NULL OR r.temperature_apache_raw = -1 THEN 1 ELSE 0 END AS temperature_apache_missing_ind,
  CASE WHEN r.temperature_apache_raw IS NULL OR r.temperature_apache_raw = -1 THEN NULL ELSE r.temperature_apache_raw END AS temperature_apache_clean,

  CASE WHEN r.creatinine_apache_raw = -1 THEN 1 ELSE 0 END AS creatinine_apache_sentinel_ind,
  CASE WHEN r.creatinine_apache_raw IS NULL OR r.creatinine_apache_raw = -1 THEN 1 ELSE 0 END AS creatinine_apache_missing_ind,
  CASE WHEN r.creatinine_apache_raw IS NULL OR r.creatinine_apache_raw = -1 THEN NULL ELSE r.creatinine_apache_raw END AS creatinine_apache_clean,

  CASE WHEN r.wbc_apache_raw = -1 THEN 1 ELSE 0 END AS wbc_apache_sentinel_ind,
  CASE WHEN r.wbc_apache_raw IS NULL OR r.wbc_apache_raw = -1 THEN 1 ELSE 0 END AS wbc_apache_missing_ind,
  CASE WHEN r.wbc_apache_raw IS NULL OR r.wbc_apache_raw = -1 THEN NULL ELSE r.wbc_apache_raw END AS wbc_apache_clean,

  CASE WHEN r.sodium_apache_raw = -1 THEN 1 ELSE 0 END AS sodium_apache_sentinel_ind,
  CASE WHEN r.sodium_apache_raw IS NULL OR r.sodium_apache_raw = -1 THEN 1 ELSE 0 END AS sodium_apache_missing_ind,
  CASE WHEN r.sodium_apache_raw IS NULL OR r.sodium_apache_raw = -1 THEN NULL ELSE r.sodium_apache_raw END AS sodium_apache_clean,

  CASE WHEN r.glucose_apache_raw = -1 THEN 1 ELSE 0 END AS glucose_apache_sentinel_ind,
  CASE WHEN r.glucose_apache_raw IS NULL OR r.glucose_apache_raw = -1 THEN 1 ELSE 0 END AS glucose_apache_missing_ind,
  CASE WHEN r.glucose_apache_raw IS NULL OR r.glucose_apache_raw = -1 THEN NULL ELSE r.glucose_apache_raw END AS glucose_apache_clean,

  CASE WHEN r.bilirubin_apache_raw = -1 THEN 1 ELSE 0 END AS bilirubin_apache_sentinel_ind,
  CASE WHEN r.bilirubin_apache_raw IS NULL OR r.bilirubin_apache_raw = -1 THEN 1 ELSE 0 END AS bilirubin_apache_missing_ind,
  CASE WHEN r.bilirubin_apache_raw IS NULL OR r.bilirubin_apache_raw = -1 THEN NULL ELSE r.bilirubin_apache_raw END AS bilirubin_apache_clean,

  CASE WHEN r.albumin_apache_raw = -1 THEN 1 ELSE 0 END AS albumin_apache_sentinel_ind,
  CASE WHEN r.albumin_apache_raw IS NULL OR r.albumin_apache_raw = -1 THEN 1 ELSE 0 END AS albumin_apache_missing_ind,
  CASE WHEN r.albumin_apache_raw IS NULL OR r.albumin_apache_raw = -1 THEN NULL ELSE r.albumin_apache_raw END AS albumin_apache_clean,

  CASE WHEN r.pao2_apache_raw = -1 THEN 1 ELSE 0 END AS pao2_apache_sentinel_ind,
  CASE WHEN r.pao2_apache_raw IS NULL OR r.pao2_apache_raw = -1 THEN 1 ELSE 0 END AS pao2_apache_missing_ind,
  CASE WHEN r.pao2_apache_raw IS NULL OR r.pao2_apache_raw = -1 THEN NULL ELSE r.pao2_apache_raw END AS pao2_apache_clean,

  CASE WHEN r.ph_apache_raw = -1 THEN 1 ELSE 0 END AS ph_apache_sentinel_ind,
  CASE WHEN r.ph_apache_raw IS NULL OR r.ph_apache_raw = -1 THEN 1 ELSE 0 END AS ph_apache_missing_ind,
  CASE WHEN r.ph_apache_raw IS NULL OR r.ph_apache_raw = -1 THEN NULL ELSE r.ph_apache_raw END AS ph_apache_clean,

  CASE WHEN r.diabetes_pred_raw = -1 THEN 1 ELSE 0 END AS diabetes_pred_sentinel_ind,
  CASE WHEN r.diabetes_pred_raw IS NULL OR r.diabetes_pred_raw = -1 THEN 1 ELSE 0 END AS diabetes_pred_missing_ind,
  CASE WHEN r.diabetes_pred_raw IS NULL OR r.diabetes_pred_raw = -1 THEN NULL ELSE r.diabetes_pred_raw END AS diabetes_pred_clean,

  CASE WHEN r.cirrhosis_pred_raw = -1 THEN 1 ELSE 0 END AS cirrhosis_pred_sentinel_ind,
  CASE WHEN r.cirrhosis_pred_raw IS NULL OR r.cirrhosis_pred_raw = -1 THEN 1 ELSE 0 END AS cirrhosis_pred_missing_ind,
  CASE WHEN r.cirrhosis_pred_raw IS NULL OR r.cirrhosis_pred_raw = -1 THEN NULL ELSE r.cirrhosis_pred_raw END AS cirrhosis_pred_clean,

  CASE WHEN r.immunosuppression_pred_raw = -1 THEN 1 ELSE 0 END AS immunosuppression_pred_sentinel_ind,
  CASE WHEN r.immunosuppression_pred_raw IS NULL OR r.immunosuppression_pred_raw = -1 THEN 1 ELSE 0 END AS immunosuppression_pred_missing_ind,
  CASE WHEN r.immunosuppression_pred_raw IS NULL OR r.immunosuppression_pred_raw = -1 THEN NULL ELSE r.immunosuppression_pred_raw END AS immunosuppression_pred_clean,

  CASE WHEN r.aids_pred_raw = -1 THEN 1 ELSE 0 END AS aids_pred_sentinel_ind,
  CASE WHEN r.aids_pred_raw IS NULL OR r.aids_pred_raw = -1 THEN 1 ELSE 0 END AS aids_pred_missing_ind,
  CASE WHEN r.aids_pred_raw IS NULL OR r.aids_pred_raw = -1 THEN NULL ELSE r.aids_pred_raw END AS aids_pred_clean,

  CASE WHEN r.hepaticfailure_pred_raw = -1 THEN 1 ELSE 0 END AS hepaticfailure_pred_sentinel_ind,
  CASE WHEN r.hepaticfailure_pred_raw IS NULL OR r.hepaticfailure_pred_raw = -1 THEN 1 ELSE 0 END AS hepaticfailure_pred_missing_ind,
  CASE WHEN r.hepaticfailure_pred_raw IS NULL OR r.hepaticfailure_pred_raw = -1 THEN NULL ELSE r.hepaticfailure_pred_raw END AS hepaticfailure_pred_clean,

  CASE WHEN r.lymphoma_pred_raw = -1 THEN 1 ELSE 0 END AS lymphoma_pred_sentinel_ind,
  CASE WHEN r.lymphoma_pred_raw IS NULL OR r.lymphoma_pred_raw = -1 THEN 1 ELSE 0 END AS lymphoma_pred_missing_ind,
  CASE WHEN r.lymphoma_pred_raw IS NULL OR r.lymphoma_pred_raw = -1 THEN NULL ELSE r.lymphoma_pred_raw END AS lymphoma_pred_clean,

  CASE WHEN r.metastaticcancer_pred_raw = -1 THEN 1 ELSE 0 END AS metastaticcancer_pred_sentinel_ind,
  CASE WHEN r.metastaticcancer_pred_raw IS NULL OR r.metastaticcancer_pred_raw = -1 THEN 1 ELSE 0 END AS metastaticcancer_pred_missing_ind,
  CASE WHEN r.metastaticcancer_pred_raw IS NULL OR r.metastaticcancer_pred_raw = -1 THEN NULL ELSE r.metastaticcancer_pred_raw END AS metastaticcancer_pred_clean,

  CASE WHEN r.leukemia_pred_raw = -1 THEN 1 ELSE 0 END AS leukemia_pred_sentinel_ind,
  CASE WHEN r.leukemia_pred_raw IS NULL OR r.leukemia_pred_raw = -1 THEN 1 ELSE 0 END AS leukemia_pred_missing_ind,
  CASE WHEN r.leukemia_pred_raw IS NULL OR r.leukemia_pred_raw = -1 THEN NULL ELSE r.leukemia_pred_raw END AS leukemia_pred_clean,

  CASE WHEN r.readmit_pred_raw = -1 THEN 1 ELSE 0 END AS readmit_pred_sentinel_ind,
  CASE WHEN r.readmit_pred_raw IS NULL OR r.readmit_pred_raw = -1 THEN 1 ELSE 0 END AS readmit_pred_missing_ind,
  CASE WHEN r.readmit_pred_raw IS NULL OR r.readmit_pred_raw = -1 THEN NULL ELSE r.readmit_pred_raw END AS readmit_pred_clean,

  CASE WHEN r.ventday1_pred_raw = -1 THEN 1 ELSE 0 END AS ventday1_pred_sentinel_ind,
  CASE WHEN r.ventday1_pred_raw IS NULL OR r.ventday1_pred_raw = -1 THEN 1 ELSE 0 END AS ventday1_pred_missing_ind,
  CASE WHEN r.ventday1_pred_raw IS NULL OR r.ventday1_pred_raw = -1 THEN NULL ELSE r.ventday1_pred_raw END AS ventday1_pred_clean

FROM public.eicu_formal_v1_analysis_main_apache_raw r;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_analysis_main_apache_clean_id
ON public.eicu_formal_v1_analysis_main_apache_clean (patientunitstayid);

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  SUM(apache_iva_missing_ind) AS apache_iva_missing_n,
  SUM(apacheapsvar_missing_ind) AS apacheapsvar_missing_n,
  SUM(apachepredvar_missing_ind) AS apachepredvar_missing_n,
  SUM(aps_iva_missing_ind) AS aps_iva_missing_n,
  SUM(apache_score_iva_missing_ind) AS apache_score_iva_missing_n,
  SUM(pred_hosp_mort_iva_missing_ind) AS pred_hosp_mort_missing_n,
  SUM(heartrate_apache_missing_ind) AS hr_missing_n,
  SUM(meanbp_apache_missing_ind) AS map_missing_n,
  SUM(respiratoryrate_apache_missing_ind) AS rr_missing_n,
  SUM(temperature_apache_missing_ind) AS temp_missing_n,
  SUM(creatinine_apache_missing_ind) AS creat_missing_n,
  SUM(wbc_apache_missing_ind) AS wbc_missing_n,
  SUM(bilirubin_apache_missing_ind) AS bili_missing_n,
  SUM(albumin_apache_missing_ind) AS alb_missing_n,
  SUM(pao2_apache_missing_ind) AS pao2_missing_n,
  SUM(ph_apache_missing_ind) AS ph_missing_n,
  SUM(diabetes_pred_missing_ind) AS dm_missing_n,
  SUM(cirrhosis_pred_missing_ind) AS cirr_missing_n,
  SUM(immunosuppression_pred_missing_ind) AS immuno_missing_n
FROM public.eicu_formal_v1_analysis_main_apache_clean;


/* -------------------------------------------------------------------------
   Step 10. Hospital-level antibiotic reporting profile.

   This captures eICU hospital upload/reporting structure.
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_hospital_abx_reporting_profile;

CREATE TABLE public.eicu_formal_v1_hospital_abx_reporting_profile AS
SELECT
  hospitalid,
  COUNT(*) AS hosp_sepsis_n,
  SUM(no_abx_record) AS hosp_no_abx_n,
  SUM(main_three_window_candidate) AS hosp_main_three_window_n,
  SUM(post_t0_abx_0_72h) AS hosp_post_t0_abx_0_72h_n,
  ROUND(100.0 * SUM(no_abx_record) / COUNT(*), 1) AS hosp_no_abx_pct,
  ROUND(100.0 * SUM(main_three_window_candidate) / COUNT(*), 1) AS hosp_main_three_window_pct,
  CASE
    WHEN SUM(main_three_window_candidate) = 0 THEN '0'
    WHEN SUM(main_three_window_candidate) <= 10 THEN '1-10'
    WHEN SUM(main_three_window_candidate) <= 30 THEN '11-30'
    ELSE '>30'
  END AS hosp_main_three_window_bucket,
  CASE WHEN SUM(no_abx_record) = COUNT(*) THEN 1 ELSE 0 END AS hosp_zero_abx_reporting_ind
FROM public.eicu_formal_v1_analysis_main
GROUP BY hospitalid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_hosp_abx_profile_hosp
ON public.eicu_formal_v1_hospital_abx_reporting_profile (hospitalid);

SELECT
  COUNT(*) AS n_hosp,
  SUM(hosp_zero_abx_reporting_ind) AS hosp_zero_abx_reporting,
  SUM((hosp_main_three_window_n > 0)::int) AS hosp_with_any_window,
  ROUND(100.0 * SUM(hosp_no_abx_n) / SUM(hosp_sepsis_n), 1) AS overall_no_abx_pct,
  ROUND(AVG(hosp_no_abx_pct), 1) AS mean_within_hosp_no_abx_pct
FROM public.eicu_formal_v1_hospital_abx_reporting_profile;

SELECT
  hosp_main_three_window_bucket AS bucket,
  COUNT(*) AS n_hosp,
  SUM(hosp_main_three_window_n) AS total_window_pts
FROM public.eicu_formal_v1_hospital_abx_reporting_profile
GROUP BY hosp_main_three_window_bucket
ORDER BY
  CASE hosp_main_three_window_bucket
    WHEN '0' THEN 1
    WHEN '1-10' THEN 2
    WHEN '11-30' THEN 3
    WHEN '>30' THEN 4
    ELSE 9
  END;


/* -------------------------------------------------------------------------
   Step 11. Final model-ready wide table.

   Output:
     public.eicu_formal_v1_analysis_main_model_ready_wide

   Includes:
     - full strict-sepsis first-ICU cohort
     - arm2 and arm3 labels
     - hospital reporting profile
     - APACHE clean variables
     - physiologic-range final variables
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_analysis_main_model_ready_wide;

CREATE TABLE public.eicu_formal_v1_analysis_main_model_ready_wide AS
SELECT
  c.*,

  h.hosp_sepsis_n,
  h.hosp_no_abx_n,
  h.hosp_main_three_window_n,
  h.hosp_post_t0_abx_0_72h_n,
  h.hosp_no_abx_pct,
  h.hosp_main_three_window_pct,
  h.hosp_main_three_window_bucket,
  h.hosp_zero_abx_reporting_ind,

  CASE WHEN c.death_30d_inhosp IS NOT NULL THEN 1 ELSE 0 END AS outcome_available_ind,

  CASE
    WHEN c.abx_window_unit_t0 IN ('0-3h', '3-6h', '6-12h') THEN 1 ELSE 0
  END AS arm2_candidate_ind,

  CASE
    WHEN c.abx_window_unit_t0 IN ('0-3h', '3-6h', '6-12h')
         AND c.death_30d_inhosp IS NOT NULL
      THEN 1 ELSE 0
  END AS arm2_model_ind,

  CASE
    WHEN c.abx_window_unit_t0 = '0-3h' THEN '0-3h'
    WHEN c.abx_window_unit_t0 IN ('3-6h', '6-12h') THEN '3-12h'
    ELSE NULL
  END AS arm2_abx_timing,

  CASE
    WHEN c.abx_window_unit_t0 = '0-3h' THEN 0
    WHEN c.abx_window_unit_t0 IN ('3-6h', '6-12h') THEN 1
    ELSE NULL
  END AS arm2_late_3_12h,

  CASE
    WHEN c.abx_window_unit_t0 IN ('0-3h', '3-6h', '6-12h')
         AND c.death_30d_inhosp IS NOT NULL
      THEN c.abx_window_unit_t0
    ELSE NULL
  END AS arm3_abx_timing,

  CASE
    WHEN c.abx_window_unit_t0 IN ('0-3h', '3-6h', '6-12h', '>12-72h')
         AND c.death_30d_inhosp IS NOT NULL
      THEN 1 ELSE 0
  END AS post_t0_0_72h_model_ind,

  CASE
    WHEN c.first_abx_offset_min < -4320 THEN '<-72h'
    WHEN c.first_abx_offset_min >= -4320 AND c.first_abx_offset_min < -1440 THEN '-72h to -24h'
    WHEN c.first_abx_offset_min >= -1440 AND c.first_abx_offset_min < -720 THEN '-24h to -12h'
    WHEN c.first_abx_offset_min >= -720 AND c.first_abx_offset_min < -360 THEN '-12h to -6h'
    WHEN c.first_abx_offset_min >= -360 AND c.first_abx_offset_min < -180 THEN '-6h to -3h'
    WHEN c.first_abx_offset_min >= -180 AND c.first_abx_offset_min < 0 THEN '-3h to 0h'
    ELSE NULL
  END AS pre_t0_offset_bin,

  CASE WHEN c.first_abx_offset_min < -4320 THEN 1 ELSE 0 END AS remote_pre_icu_abx_lt_72h_ind,

  CASE WHEN c.heartrate_apache_clean IS NOT NULL AND (c.heartrate_apache_clean < 20 OR c.heartrate_apache_clean > 300) THEN 1 ELSE 0 END AS heartrate_apache_range_outlier_ind,
  CASE WHEN c.meanbp_apache_clean IS NOT NULL AND (c.meanbp_apache_clean < 20 OR c.meanbp_apache_clean > 250) THEN 1 ELSE 0 END AS meanbp_apache_range_outlier_ind,
  CASE WHEN c.respiratoryrate_apache_clean IS NOT NULL AND (c.respiratoryrate_apache_clean < 1 OR c.respiratoryrate_apache_clean > 80) THEN 1 ELSE 0 END AS respiratoryrate_apache_range_outlier_ind,
  CASE WHEN c.temperature_apache_clean IS NOT NULL AND (c.temperature_apache_clean < 25 OR c.temperature_apache_clean > 45) THEN 1 ELSE 0 END AS temperature_apache_range_outlier_ind,
  CASE WHEN c.creatinine_apache_clean IS NOT NULL AND (c.creatinine_apache_clean < 0.1 OR c.creatinine_apache_clean > 30) THEN 1 ELSE 0 END AS creatinine_apache_range_outlier_ind,
  CASE WHEN c.wbc_apache_clean IS NOT NULL AND (c.wbc_apache_clean < 0.1 OR c.wbc_apache_clean > 500) THEN 1 ELSE 0 END AS wbc_apache_range_outlier_ind,
  CASE WHEN c.sodium_apache_clean IS NOT NULL AND (c.sodium_apache_clean < 90 OR c.sodium_apache_clean > 190) THEN 1 ELSE 0 END AS sodium_apache_range_outlier_ind,
  CASE WHEN c.glucose_apache_clean IS NOT NULL AND (c.glucose_apache_clean < 20 OR c.glucose_apache_clean > 1000) THEN 1 ELSE 0 END AS glucose_apache_range_outlier_ind,

  CASE WHEN c.heartrate_apache_clean BETWEEN 20 AND 300 THEN c.heartrate_apache_clean ELSE NULL END AS heartrate_apache_final,
  CASE WHEN c.meanbp_apache_clean BETWEEN 20 AND 250 THEN c.meanbp_apache_clean ELSE NULL END AS meanbp_apache_final,
  CASE WHEN c.respiratoryrate_apache_clean BETWEEN 1 AND 80 THEN c.respiratoryrate_apache_clean ELSE NULL END AS respiratoryrate_apache_final,
  CASE WHEN c.temperature_apache_clean BETWEEN 25 AND 45 THEN c.temperature_apache_clean ELSE NULL END AS temperature_apache_final,
  CASE WHEN c.creatinine_apache_clean BETWEEN 0.1 AND 30 THEN c.creatinine_apache_clean ELSE NULL END AS creatinine_apache_final,
  CASE WHEN c.wbc_apache_clean BETWEEN 0.1 AND 500 THEN c.wbc_apache_clean ELSE NULL END AS wbc_apache_final,
  CASE WHEN c.sodium_apache_clean BETWEEN 90 AND 190 THEN c.sodium_apache_clean ELSE NULL END AS sodium_apache_final,
  CASE WHEN c.glucose_apache_clean BETWEEN 20 AND 1000 THEN c.glucose_apache_clean ELSE NULL END AS glucose_apache_final,

  CASE WHEN c.heartrate_apache_clean BETWEEN 20 AND 300 THEN 0 ELSE 1 END AS heartrate_apache_final_missing_ind,
  CASE WHEN c.meanbp_apache_clean BETWEEN 20 AND 250 THEN 0 ELSE 1 END AS meanbp_apache_final_missing_ind,
  CASE WHEN c.respiratoryrate_apache_clean BETWEEN 1 AND 80 THEN 0 ELSE 1 END AS respiratoryrate_apache_final_missing_ind,
  CASE WHEN c.temperature_apache_clean BETWEEN 25 AND 45 THEN 0 ELSE 1 END AS temperature_apache_final_missing_ind,
  CASE WHEN c.creatinine_apache_clean BETWEEN 0.1 AND 30 THEN 0 ELSE 1 END AS creatinine_apache_final_missing_ind,
  CASE WHEN c.wbc_apache_clean BETWEEN 0.1 AND 500 THEN 0 ELSE 1 END AS wbc_apache_final_missing_ind,
  CASE WHEN c.sodium_apache_clean BETWEEN 90 AND 190 THEN 0 ELSE 1 END AS sodium_apache_final_missing_ind,
  CASE WHEN c.glucose_apache_clean BETWEEN 20 AND 1000 THEN 0 ELSE 1 END AS glucose_apache_final_missing_ind

FROM public.eicu_formal_v1_analysis_main_apache_clean c
LEFT JOIN public.eicu_formal_v1_hospital_abx_reporting_profile h
  ON h.hospitalid = c.hospitalid;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_model_ready_wide_stay
ON public.eicu_formal_v1_analysis_main_model_ready_wide (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_model_ready_wide_hosp
ON public.eicu_formal_v1_analysis_main_model_ready_wide (hospitalid);

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  SUM((death_30d_inhosp IS NULL)::int) AS outcome_missing_n,
  SUM(arm2_candidate_ind) AS arm2_candidate_n,
  SUM(arm2_model_ind) AS arm2_model_n,
  SUM(post_t0_0_72h_model_ind) AS post_t0_0_72h_model_n,
  SUM(glucose_apache_range_outlier_ind) AS glucose_outlier_n,
  SUM(temperature_apache_range_outlier_ind) AS temp_outlier_n,
  SUM(wbc_apache_range_outlier_ind) AS wbc_outlier_n
FROM public.eicu_formal_v1_analysis_main_model_ready_wide;

SELECT
  arm2_abx_timing,
  COUNT(*) AS n,
  SUM(death_30d_inhosp) AS death_30d_inhosp_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / COUNT(*), 2) AS death_30d_inhosp_pct
FROM public.eicu_formal_v1_analysis_main_model_ready_wide
WHERE arm2_model_ind = 1
GROUP BY arm2_abx_timing
ORDER BY arm2_abx_timing;


/* -------------------------------------------------------------------------
   Step 12. Arm2 main model dataset.

   Output:
     public.eicu_formal_v1_arm2_model_dataset

   Expected current result:
     N = 3225
     deaths = 651
     hospitals = 121
     0-3h = 2046
     3-12h = 1179

   Missing-data rule:
     This table is MICE-ready. Numeric and binary covariates are not median-
     or zero-imputed in SQL. Clean values and missing indicators are kept, and
     imputation is deferred to the R MICE step.
------------------------------------------------------------------------- */

DROP TABLE IF EXISTS public.eicu_formal_v1_arm2_model_dataset;

CREATE TABLE public.eicu_formal_v1_arm2_model_dataset AS
WITH base AS (
  SELECT *
  FROM public.eicu_formal_v1_analysis_main_model_ready_wide
  WHERE arm2_model_ind = 1
)
SELECT
  b.patientunitstayid,
  b.uniquepid,
  b.hospitalid,

  b.death_30d_inhosp,
  b.arm2_abx_timing,
  b.arm2_late_3_12h,

  b.first_abx_offset_min,
  b.first_abx_delay_hours_from_unit,

  b.age_num AS age_raw,
  b.age_num::double precision AS age_model,
  CASE WHEN b.age_num IS NULL THEN 1 ELSE 0 END AS age_missing_ind,

  COALESCE(NULLIF(b.gender, ''), 'Missing') AS gender_model,
  COALESCE(NULLIF(b.ethnicity, ''), 'Missing') AS ethnicity_model,
  COALESCE(NULLIF(b.unitadmitsource, ''), 'Missing') AS unit_admit_source_model,
  COALESCE(NULLIF(b.unittype, ''), 'Missing') AS unit_type_model,

  b.apache_score_iva_clean AS apache_score_iva_raw_model,
  b.apache_score_iva_clean::double precision AS apache_score_iva_model,
  b.apache_score_iva_missing_ind,

  b.vent_apache_clean AS vent_model,
  b.vent_apache_missing_ind,

  b.intubated_apache_clean AS intubated_model,
  b.intubated_apache_missing_ind,

  b.dialysis_apache_clean AS dialysis_model,
  b.dialysis_apache_missing_ind,

  b.diabetes_pred_clean AS diabetes_model,
  b.diabetes_pred_missing_ind,

  b.cirrhosis_pred_clean AS cirrhosis_model,
  b.cirrhosis_pred_missing_ind,

  b.immunosuppression_pred_clean AS immunosuppression_model,
  b.immunosuppression_pred_missing_ind,

  b.hepaticfailure_pred_clean AS hepaticfailure_model,
  b.hepaticfailure_pred_missing_ind,

  b.metastaticcancer_pred_clean AS metastaticcancer_model,
  b.metastaticcancer_pred_missing_ind,

  b.hosp_sepsis_n,
  b.hosp_main_three_window_n,
  b.hosp_main_three_window_bucket,
  b.hosp_zero_abx_reporting_ind

FROM base b;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_arm2_model_dataset_stay
ON public.eicu_formal_v1_arm2_model_dataset (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_arm2_model_dataset_hosp
ON public.eicu_formal_v1_arm2_model_dataset (hospitalid);

SELECT
  COUNT(*) AS model_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT hospitalid) AS n_hosp,
  SUM(death_30d_inhosp) AS death_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / COUNT(*), 2) AS death_pct,
  SUM((arm2_late_3_12h = 0)::int) AS early_0_3h_n,
  SUM((arm2_late_3_12h = 1)::int) AS late_3_12h_n,
  SUM(apache_score_iva_missing_ind) AS apache_score_missing_n,
  SUM(vent_apache_missing_ind) AS vent_missing_n,
  SUM(diabetes_pred_missing_ind) AS diabetes_missing_n
FROM public.eicu_formal_v1_arm2_model_dataset;

SELECT
  arm2_abx_timing,
  COUNT(*) AS n,
  SUM(death_30d_inhosp) AS death_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / COUNT(*), 2) AS death_pct
FROM public.eicu_formal_v1_arm2_model_dataset
GROUP BY arm2_abx_timing
ORDER BY arm2_abx_timing;


/* -------------------------------------------------------------------------
   Step 13. Hospital x arm2 separation audit.
------------------------------------------------------------------------- */

WITH h AS (
  SELECT
    hospitalid,
    COUNT(*) AS n,
    SUM((arm2_abx_timing = '0-3h')::int) AS early_0_3h_n,
    SUM((arm2_abx_timing = '3-12h')::int) AS late_3_12h_n,
    SUM(death_30d_inhosp) AS death_n,
    ROUND(100.0 * SUM(death_30d_inhosp) / COUNT(*), 2) AS death_pct
  FROM public.eicu_formal_v1_analysis_main_model_ready_wide
  WHERE arm2_model_ind = 1
  GROUP BY hospitalid
)
SELECT
  COUNT(*) AS n_hosp,
  SUM((early_0_3h_n = 0 OR late_3_12h_n = 0)::int) AS single_arm_hosp_n,
  SUM((n >= 10 AND LEAST(early_0_3h_n, late_3_12h_n)::numeric / n < 0.05)::int) AS near_single_arm_hosp_ge10_n,
  SUM(n) AS total_arm2_n,
  SUM(early_0_3h_n) AS total_early_n,
  SUM(late_3_12h_n) AS total_late_n
FROM h;


/* End of eICU formal_v1 wide-table build SQL. */
