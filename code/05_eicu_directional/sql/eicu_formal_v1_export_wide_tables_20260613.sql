/* eICU formal_v1 export wide tables, 2026-06-13

Use in Navicat:
  1. Run one SELECT block at a time.
  2. In the result grid, right click -> Export Result.
  3. Export as CSV to:
     [local path omitted]

Important:
  Do not use PostgreSQL server-side COPY here. COPY asks the PostgreSQL server
  process to write the file, and it may not see the local Windows path.
*/


/* 00. QC before export.

Expected:
  eicu_formal_v1_analysis_main_model_ready_wide: rows_n = 21326
  eicu_formal_v1_arm2_model_dataset: rows_n = 3225
*/

SELECT
  'eicu_formal_v1_analysis_main_model_ready_wide' AS table_name,
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays
FROM public.eicu_formal_v1_analysis_main_model_ready_wide

UNION ALL

SELECT
  'eicu_formal_v1_arm2_model_dataset' AS table_name,
  COUNT(*) AS rows_n,
  COUNT(DISTINCT patientunitstayid) AS distinct_stays
FROM public.eicu_formal_v1_arm2_model_dataset;


/* 01. Full final model-ready wide table.

Export file name:
  eicu_formal_v1_analysis_main_model_ready_wide.csv

Content:
  One row per patientunitstayid.
  Full strict-sepsis first-ICU cohort.
  Includes exposure windows, outcome, demographics, APACHE raw/clean variables,
  missing/sentinel indicators, hospital reporting profile, and arm labels.

Expected N:
  21326
*/

SELECT *
FROM public.eicu_formal_v1_analysis_main_model_ready_wide
ORDER BY patientunitstayid;


/* 02. Main binary-arm model dataset.

Export file name:
  eicu_formal_v1_arm2_model_dataset.csv

Content:
  Main model analysis set only.
  arm2 = 0-3h vs 3-12h.
  Outcome non-missing.
  Includes model-ready covariates.

Expected N:
  3225
*/

SELECT *
FROM public.eicu_formal_v1_arm2_model_dataset
ORDER BY patientunitstayid;


/* 03. Table 1 continuous summary.

Export file name:
  eicu_formal_v1_table1_arm2_continuous.csv
*/

SELECT *
FROM public.eicu_formal_v1_table1_arm2_continuous
ORDER BY variable, group_name;


/* 04. Table 1 categorical summary.

Export file name:
  eicu_formal_v1_table1_arm2_categorical.csv
*/

SELECT *
FROM public.eicu_formal_v1_table1_arm2_categorical
ORDER BY variable, level, group_name;


/* 05. Table 1 continuous SMD.

Export file name:
  eicu_formal_v1_table1_arm2_smd_continuous.csv
*/

SELECT *
FROM public.eicu_formal_v1_table1_arm2_smd_continuous
ORDER BY abs_smd DESC;


/* 06. Table 1 categorical max SMD.

Export file name:
  eicu_formal_v1_table1_arm2_smd_categorical_max.csv
*/

SELECT *
FROM public.eicu_formal_v1_table1_arm2_smd_categorical_max
ORDER BY max_abs_smd DESC;


/* 07. Table 1 categorical level SMD.

Export file name:
  eicu_formal_v1_table1_arm2_smd_categorical_level.csv
*/

SELECT *
FROM public.eicu_formal_v1_table1_arm2_smd_categorical_level
ORDER BY variable, abs_smd DESC, level;


/* Optional psql-only client-side export.

Do not run the following lines in Navicat. They are for psql only.
If using psql, remove the leading comment markers and run them there.

\copy (SELECT * FROM public.eicu_formal_v1_analysis_main_model_ready_wide ORDER BY patientunitstayid) TO 'D:/codex 宸ヤ綔鐩綍/TTE鐩爣妯℃嫙瀹為獙/TTE姝ｅ紡瀹為獙/eICU姝ｅ紡娴佺▼_formal_v1/outputs/wide_tables/eicu_formal_v1_analysis_main_model_ready_wide.csv' WITH CSV HEADER;

\copy (SELECT * FROM public.eicu_formal_v1_arm2_model_dataset ORDER BY patientunitstayid) TO 'D:/codex 宸ヤ綔鐩綍/TTE鐩爣妯℃嫙瀹為獙/TTE姝ｅ紡瀹為獙/eICU姝ｅ紡娴佺▼_formal_v1/outputs/wide_tables/eicu_formal_v1_arm2_model_dataset.csv' WITH CSV HEADER;
*/

