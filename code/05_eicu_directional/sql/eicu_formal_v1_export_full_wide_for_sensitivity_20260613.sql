/* eICU formal_v1 full wide export for sensitivity analyses, 2026-06-13

Purpose:
  Export the FULL model-ready wide table for analyses that need windows outside
  the main arm2 set, especially >12-72h vs 0-3h sensitivity.

Use in Navicat:
  1. Run block 00 first.
  2. Confirm rows_n = 21,326 and distinct_stays = 21,326.
  3. Run block 01.
  4. Right click the result grid -> Export Result -> CSV.
  5. Save exactly as:

     [local path omitted]

Important:
  Do NOT export public.eicu_formal_v1_arm2_model_dataset to this filename.
  That arm2 table has only 3,225 rows and cannot support >12-72h sensitivity.
*/


/* 00. Full wide QC. Expected rows_n = 21,326. */

SELECT
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays,
  SUM((death_30d_inhosp IS NULL)::int) AS outcome_missing_n,
  SUM((abx_window_unit_t0 = '0-3h')::int) AS window_0_3h_n,
  SUM((abx_window_unit_t0 = '3-6h')::int) AS window_3_6h_n,
  SUM((abx_window_unit_t0 = '6-12h')::int) AS window_6_12h_n,
  SUM((abx_window_unit_t0 = '>12-72h')::int) AS window_gt12_72h_n,
  SUM((abx_window_unit_t0 = '>72h')::int) AS window_gt72h_n,
  SUM((abx_window_unit_t0 = 'pre_t0_abx')::int) AS pre_t0_abx_n,
  SUM((abx_window_unit_t0 = 'no_abx_record')::int) AS no_abx_record_n
FROM public.eicu_formal_v1_analysis_main_model_ready_wide;


/* 01. Export this result grid as CSV to the exact path listed above. */

SELECT *
FROM public.eicu_formal_v1_analysis_main_model_ready_wide
ORDER BY patientunitstayid;


/* 02. Optional after export: expected sensitivity raw counts.
   This is just for visual QC in Navicat.
*/

SELECT
  abx_window_unit_t0,
  COUNT(*) AS n,
  SUM((death_30d_inhosp = 1)::int) AS death_30d_inhosp_n,
  ROUND(
    100.0 * SUM((death_30d_inhosp = 1)::int)
      / NULLIF(SUM((death_30d_inhosp IS NOT NULL)::int), 0),
    2
  ) AS death_30d_inhosp_pct_nonmissing
FROM public.eicu_formal_v1_analysis_main_model_ready_wide
WHERE abx_window_unit_t0 IN ('0-3h', '>12-72h')
GROUP BY abx_window_unit_t0
ORDER BY
  CASE abx_window_unit_t0
    WHEN '0-3h' THEN 1
    WHEN '>12-72h' THEN 2
    ELSE 99
  END;

