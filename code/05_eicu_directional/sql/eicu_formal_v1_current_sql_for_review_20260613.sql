/* eICU formal_v1 current SQL for review, 2026-06-13

Scope covered in this file:
  1. First observed ICU stay per uniquepid.
  2. Adult first ICU stay.
  3. Strict explicit sepsis label within adult first ICU stays.
  4. First antibiotic timing and ICU-admission anchored exposure windows.
  5. Exposure-outcome backbone wide table: public.eicu_formal_v1_analysis_main.

Scope not yet covered:
  - APACHE covariate merge.
  - raw/clean/missing-indicator covariate processing.
  - Table 1.
  - long table / CCW / IPCW.

Study positioning:
  - eICU directional external validation.
  - t0 = ICU admission offset 0.
  - outcome = 30-day in-hospital mortality, not 30-day all-cause mortality.
*/


/* Optional speed indexes. These do not change data values. */

CREATE INDEX IF NOT EXISTS idx_eicu_diagnosis_stay
ON eicu_crd.diagnosis (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_admissiondx_stay
ON eicu_crd.admissiondx (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_medication_stay
ON eicu_crd.medication (patientunitstayid);

CREATE INDEX IF NOT EXISTS idx_eicu_infusiondrug_stay
ON eicu_crd.infusiondrug (patientunitstayid);


/* Step 01. First observed ICU stay per uniquepid. */

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

/* QC expected from current run:
   rows_n = 139367
   distinct_stays = 139367
   distinct_uniquepid = 139367
   adult_n = 138855
   age_missing_n = 80
*/

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  SUM(CASE WHEN age_num >= 18 THEN 1 ELSE 0 END) AS adult_n,
  SUM(CASE WHEN age_num IS NULL THEN 1 ELSE 0 END) AS age_missing_n
FROM public.eicu_formal_v1_first_icu_stay;

SELECT
  unitvisitnumber,
  COUNT(*) AS n
FROM public.eicu_formal_v1_first_icu_stay
GROUP BY unitvisitnumber
ORDER BY unitvisitnumber;


/* Step 02. Adult first observed ICU stay. */

DROP TABLE IF EXISTS public.eicu_formal_v1_adult_first_icu_stay;

CREATE TABLE public.eicu_formal_v1_adult_first_icu_stay AS
SELECT *
FROM public.eicu_formal_v1_first_icu_stay
WHERE age_num >= 18;

CREATE INDEX IF NOT EXISTS idx_eicu_formal_v1_adult_first_icu_stay_id
ON public.eicu_formal_v1_adult_first_icu_stay (patientunitstayid);

/* QC expected from current run:
   rows_n = 138855
   distinct_stays = 138855
   distinct_uniquepid = 138855
   min_age = 18
   max_age = 90
*/

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  MIN(age_num) AS min_age,
  MAX(age_num) AS max_age
FROM public.eicu_formal_v1_adult_first_icu_stay;


/* Step 03. Sepsis labels within adult first observed ICU stays. */

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


/* Step 04. Strict explicit sepsis adult first ICU cohort. */

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

/* QC expected from current run:
   strict_sepsis_first_icu_n = 21326
   distinct_stays = 21326
   distinct_uniquepid = 21326
   has_diagnosis_sepsis_n = 17867
   has_admissiondx_sepsis_n = 17013
*/

SELECT
  COUNT(*) AS strict_sepsis_first_icu_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  SUM(CASE WHEN has_diagnosis_sepsis THEN 1 ELSE 0 END) AS has_diagnosis_sepsis_n,
  SUM(CASE WHEN has_admissiondx_sepsis THEN 1 ELSE 0 END) AS has_admissiondx_sepsis_n
FROM public.eicu_formal_v1_strict_sepsis_adult_first_icu;


/* Step 05. Antibiotic candidate records within strict sepsis adult first ICU cohort. */

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


/* Step 06. First antibiotic record per strict sepsis stay. */

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

/* QC expected from current run:
   pre_t0_abx = 7038
   0-3h = 2074
   3-6h = 696
   6-12h = 491
   >12-72h = 1035
   >72h = 321
   no_abx_record = 9671
*/

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


/* Step 07. Exposure-outcome backbone wide table. */

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

  CASE
    WHEN f.first_abx_offset_min < 0 THEN 1 ELSE 0
  END AS pre_t0_abx,

  CASE
    WHEN f.first_abx_offset_min IS NULL THEN 1 ELSE 0
  END AS no_abx_record,

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

/* QC expected from current run:
   rows_n = 21326
   distinct_stays = 21326
   distinct_uniquepid = 21326
   death_30d_inhosp_n = 4027
   death_30d_inhosp_pct = 18.88
   pre_t0_abx_n = 7038
   no_abx_record_n = 9671
   main_three_window_candidate_n = 3261
   post_t0_abx_0_72h_n = 4296
*/

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  COUNT(DISTINCT uniquepid) AS distinct_uniquepid,
  SUM(death_30d_inhosp) AS death_30d_inhosp_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / COUNT(*), 2) AS death_30d_inhosp_pct,
  SUM(pre_t0_abx) AS pre_t0_abx_n,
  SUM(no_abx_record) AS no_abx_record_n,
  SUM(main_three_window_candidate) AS main_three_window_candidate_n,
  SUM(post_t0_abx_0_72h) AS post_t0_abx_0_72h_n
FROM public.eicu_formal_v1_analysis_main;

SELECT
  abx_window_unit_t0,
  COUNT(*) AS n,
  SUM(death_30d_inhosp) AS death_30d_inhosp_n,
  ROUND(100.0 * SUM(death_30d_inhosp) / COUNT(*), 2) AS death_30d_inhosp_pct
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
