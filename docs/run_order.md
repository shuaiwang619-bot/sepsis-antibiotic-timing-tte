# Run order

This is the conceptual run order for credentialed users with local access to MIMIC-IV and eICU. Paths and database connection details must be adapted locally.

1. Run MIMIC SQL extraction scripts in `code/01_mimic_sql_extraction`.
2. Run MIMIC cleaning and imputation scripts in `code/02_mimic_cleaning_imputation` in numeric order.
3. Run CCW/IPCW analysis scripts in `code/03_mimic_ccw_ipcw_main` in numeric order.
4. Run post-hoc exploratory scripts in `code/04_sensitivity_posthoc` as needed.
5. Run eICU directional exploration scripts in `code/05_eicu_directional`.
6. Generate tables and figures using `code/06_tables_figures`.

The public repository does not include patient-level intermediate files required as script inputs; these must be regenerated locally from credentialed databases.

## Notes

- The MIMIC-IV primary analysis uses suspected-infection time zero and three CCW strategies: 0-3 h, 3-6 h, and 6-12 h.
- The eICU analysis is a directional exploration anchored at ICU admission, not a same-definition replication.
- Aggregate outputs in `outputs_aggregate/` can be used to verify manuscript estimates without redistributing patient-level data.
