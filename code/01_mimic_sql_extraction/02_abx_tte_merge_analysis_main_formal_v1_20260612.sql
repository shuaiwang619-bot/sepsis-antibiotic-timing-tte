/* FORMAL_V1 analysis_main merge script, 2026-06-12.
   Primary path: run 01_abx_tte_full_pipeline_formal_v1_20260612.sql from scratch first.
   This script rebuilds analysis_main inside abx_tte_formal_v1 when cov_base/cohort_exposure/outcomes already exist.

   Safety changes after review:
   - Do not use b.*. cov_base columns are listed explicitly to avoid silent duplicate-name collisions with outcomes.
   - outcomes exposure columns abx_window/ccw_role are intentionally not selected; cov_base/cohort_exposure is the exposure source of truth.
   - Boundary rows are hard-checked: delay=3 -> 0-3h, delay=6 -> 3-6h, delay=12 -> 6-12h.
   - abx_window_num is a category code, not a continuous covariate. R models must filter analyzable=1 and treat it as factor if used.
*/

/* ===== STEP A: ensure cov_base has exposure columns, then sync from cohort_exposure ===== */
ALTER TABLE abx_tte_formal_v1.cov_base
  ADD COLUMN IF NOT EXISTS abx_window            text,
  ADD COLUMN IF NOT EXISTS ccw_role              text,
  ADD COLUMN IF NOT EXISTS abx_delay_hours       numeric,
  ADD COLUMN IF NOT EXISTS first_abx_pre_72h     timestamp,
  ADD COLUMN IF NOT EXISTS first_abx_pre_earlier timestamp,
  ADD COLUMN IF NOT EXISTS first_abx_post_72h    timestamp;

UPDATE abx_tte_formal_v1.cov_base b
SET
  abx_window            = ce.abx_window,
  ccw_role              = ce.ccw_role,
  abx_delay_hours       = ce.abx_delay_hours,
  first_abx_pre_72h     = ce.first_abx_pre_72h,
  first_abx_pre_earlier = ce.first_abx_pre_earlier,
  first_abx_post_72h    = ce.first_abx_post_72h
FROM abx_tte_formal_v1.cohort_exposure ce
WHERE ce.stay_id = b.stay_id;

SELECT
  COUNT(*) AS n,
  COUNT(*) FILTER (WHERE abx_window IS NULL) AS null_abx_window,
  COUNT(*) FILTER (WHERE ccw_role   IS NULL) AS null_ccw_role
FROM abx_tte_formal_v1.cov_base;

DO $$
DECLARE
  bad_n int;
BEGIN
  SELECT CASE WHEN COUNT(*) = 24735
                   AND COUNT(*) FILTER (WHERE abx_window IS NULL) = 0
                   AND COUNT(*) FILTER (WHERE ccw_role IS NULL) = 0
              THEN 0 ELSE 1 END
  INTO bad_n
  FROM abx_tte_formal_v1.cov_base;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 merge hard check failed: cov_base exposure columns are incomplete.';
  END IF;
END $$;

/* ===== STEP B: rebuild analysis_main with explicit cov_base columns ===== */
DROP TABLE IF EXISTS abx_tte_formal_v1.analysis_main;

CREATE TABLE abx_tte_formal_v1.analysis_main AS
SELECT
  /* keys and time anchors */
  b.stay_id,
  b.subject_id,
  b.hadm_id,
  b.icu_intime,
  b.icu_outtime,
  b.sepsis_t0,

  /* demographics and admission context */
  b.age_at_icu,
  b.gender,
  b.race,
  b.admission_type,
  b.admission_location,
  b.discharge_location,
  b.weekend_admission,

  /* Charlson */
  b.charlson_comorbidity_index,
  b.age_score,
  b.myocardial_infarct,
  b.congestive_heart_failure,
  b.peripheral_vascular_disease,
  b.cerebrovascular_disease,
  b.dementia,
  b.chronic_pulmonary_disease,
  b.rheumatic_disease,
  b.peptic_ulcer_disease,
  b.mild_liver_disease,
  b.severe_liver_disease,
  b.diabetes_without_cc,
  b.diabetes_with_cc,
  b.paraplegia,
  b.renal_disease,
  b.malignant_cancer,
  b.metastatic_solid_tumor,
  b.aids,

  /* formal_v1 baseline SOFA and shock */
  b.sofa_total_t0,
  b.respiration_24hours_t0,
  b.coagulation_24hours_t0,
  b.liver_24hours_t0,
  b.cardiovascular_24hours_t0,
  b.cns_24hours_t0,
  b.renal_24hours_t0,
  b.meanbp_min_t0,
  b.gcs_min_t0,
  b.uo_24hr_t0,
  b.shock_t0_b,
  b.sofa_source_flag,

  /* exposure source of truth: cov_base/cohort_exposure */
  b.abx_window,
  b.ccw_role,
  b.first_abx_pre_72h,
  b.first_abx_pre_earlier,
  b.first_abx_post_72h,
  b.abx_delay_hours,

  /* outcomes: do not select o.abx_window or o.ccw_role to avoid duplicate/ambiguous exposure columns */
  o.dod,
  o.dischtime,
  o.death_30d,
  o.fu_status_30d,
  o.fu_time_30d_days,
  o.death_site,
  o.death_icu,
  o.death_hospital,
  o.sofa_peak_72h,
  o.organ_deterioration_sofa,
  o.icu_los_from_t0_days,
  CASE WHEN o.hosp_los_from_t0_days < 0 THEN NULL
       ELSE o.hosp_los_from_t0_days
  END AS hosp_los_from_t0_days,

  /* modeling-friendly flags. abx_window_num is categorical, not continuous. */
  (b.ccw_role = 'analyzable')::int AS analyzable,
  CASE b.abx_window
    WHEN '0-3h'  THEN 0
    WHEN '3-6h'  THEN 1
    WHEN '6-12h' THEN 2
    WHEN '>=12h' THEN 3
    WHEN 'no_iv_in_72h_after_t0' THEN 4
    WHEN 'pre_t0' THEN 4
    WHEN 'pre_t0_earlier_only' THEN 4
    ELSE NULL
  END AS abx_window_num
FROM abx_tte_formal_v1.cov_base b
LEFT JOIN abx_tte_formal_v1.outcomes o
  ON o.stay_id = b.stay_id;

CREATE INDEX ix_am_stay       ON abx_tte_formal_v1.analysis_main (stay_id);
CREATE INDEX ix_am_subject    ON abx_tte_formal_v1.analysis_main (subject_id);
CREATE INDEX ix_am_role       ON abx_tte_formal_v1.analysis_main (ccw_role);
CREATE INDEX ix_am_window     ON abx_tte_formal_v1.analysis_main (abx_window);
CREATE INDEX ix_am_analyzable ON abx_tte_formal_v1.analysis_main (analyzable);

/* ===== STEP C: validation and hard checks ===== */

SELECT COUNT(*) AS n,
       COUNT(DISTINCT stay_id) AS n_stay,
       COUNT(DISTINCT subject_id) AS n_subj
FROM abx_tte_formal_v1.analysis_main;

SELECT
  COUNT(*) FILTER (WHERE abx_window IS NULL) AS null_abx_window,
  COUNT(*) FILTER (WHERE ccw_role   IS NULL) AS null_ccw_role,
  COUNT(*) FILTER (WHERE death_30d  IS NULL) AS null_death_30d
FROM abx_tte_formal_v1.analysis_main;

SELECT ccw_role, COUNT(*) AS n
FROM abx_tte_formal_v1.analysis_main
GROUP BY ccw_role
ORDER BY ccw_role;

SELECT abx_window, COUNT(*) AS n
FROM abx_tte_formal_v1.analysis_main
GROUP BY abx_window
ORDER BY CASE abx_window
  WHEN 'pre_t0' THEN 1 WHEN 'pre_t0_earlier_only' THEN 2
  WHEN '0-3h' THEN 3 WHEN '3-6h' THEN 4 WHEN '6-12h' THEN 5
  WHEN '>=12h' THEN 6 WHEN 'no_iv_in_72h_after_t0' THEN 7 ELSE 9 END;

SELECT abx_delay_hours, abx_window, abx_window_num, ccw_role, COUNT(*) AS n
FROM abx_tte_formal_v1.analysis_main
WHERE abx_delay_hours IN (3,6,12)
GROUP BY abx_delay_hours, abx_window, abx_window_num, ccw_role
ORDER BY abx_delay_hours, abx_window, ccw_role;

SELECT COUNT(*) AS n,
       SUM(death_30d) AS n_dead,
       ROUND(100.0*SUM(death_30d)/COUNT(*),2) AS pct_death_30d
FROM abx_tte_formal_v1.analysis_main;

SELECT COUNT(*) FILTER (WHERE analyzable=1) AS n_analyzable,
       COUNT(*) FILTER (WHERE analyzable=0) AS n_not_analyzable
FROM abx_tte_formal_v1.analysis_main;

SELECT COUNT(*) FILTER (WHERE hosp_los_from_t0_days IS NULL) AS n_null_hosp_los,
       COUNT(*) FILTER (WHERE hosp_los_from_t0_days < 0)     AS n_neg_hosp_los
FROM abx_tte_formal_v1.analysis_main;

DO $$
DECLARE
  bad_n int;
BEGIN
  SELECT CASE WHEN COUNT(*) = 24735
                   AND COUNT(DISTINCT stay_id) = 24735
                   AND COUNT(DISTINCT subject_id) = 24735
                   AND COUNT(*) FILTER (WHERE abx_window IS NULL) = 0
                   AND COUNT(*) FILTER (WHERE ccw_role IS NULL) = 0
                   AND COUNT(*) FILTER (WHERE death_30d IS NULL) = 0
                   AND SUM(death_30d) = 4876
              THEN 0 ELSE 1 END
  INTO bad_n
  FROM abx_tte_formal_v1.analysis_main;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 merge hard check failed: analysis_main count/key/outcome validation failed.';
  END IF;
END $$;

DO $$
DECLARE
  bad_n int;
BEGIN
  WITH expected AS (
    SELECT '0-3h'::text AS abx_window, 3439::bigint AS expected_n
    UNION ALL SELECT '3-6h', 3241
    UNION ALL SELECT '6-12h', 5017
    UNION ALL SELECT '>=12h', 6470
  ), observed AS (
    SELECT abx_window, COUNT(*)::bigint AS observed_n
    FROM abx_tte_formal_v1.analysis_main
    WHERE ccw_role = 'analyzable'
    GROUP BY abx_window
  )
  SELECT COUNT(*) INTO bad_n
  FROM expected e
  LEFT JOIN observed o ON o.abx_window = e.abx_window
  WHERE COALESCE(o.observed_n, -1) <> e.expected_n;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 merge hard check failed: analyzable window counts drifted from 3439/3241/5017/6470.';
  END IF;
END $$;

DO $$
DECLARE
  bad_n int;
BEGIN
  WITH observed AS (
    SELECT
      COUNT(*) FILTER (WHERE abx_delay_hours = 3 AND abx_window = '0-3h' AND ccw_role='analyzable') AS delay3_ok,
      COUNT(*) FILTER (WHERE abx_delay_hours = 6 AND abx_window = '3-6h' AND ccw_role='analyzable') AS delay6_ok,
      COUNT(*) FILTER (WHERE abx_delay_hours = 12 AND abx_window = '6-12h' AND ccw_role='analyzable') AS delay12_ok,
      COUNT(*) FILTER (WHERE abx_delay_hours = 3 AND abx_window <> '0-3h' AND ccw_role='analyzable') AS delay3_bad,
      COUNT(*) FILTER (WHERE abx_delay_hours = 6 AND abx_window <> '3-6h' AND ccw_role='analyzable') AS delay6_bad,
      COUNT(*) FILTER (WHERE abx_delay_hours = 12 AND abx_window <> '6-12h' AND ccw_role='analyzable') AS delay12_bad
    FROM abx_tte_formal_v1.analysis_main
  )
  SELECT CASE WHEN delay3_ok = 106 AND delay6_ok = 136 AND delay12_ok = 69
                   AND delay3_bad = 0 AND delay6_bad = 0 AND delay12_bad = 0
              THEN 0 ELSE 1 END
  INTO bad_n
  FROM observed;

  IF bad_n > 0 THEN
    RAISE EXCEPTION 'formal_v1 merge hard check failed: delay boundary assignment is not [0,3]/(3,6]/(6,12].';
  END IF;
END $$;

SELECT * FROM abx_tte_formal_v1.analysis_main LIMIT 1;
