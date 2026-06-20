# Data dictionary, analysis-level variables

This file summarizes key analysis concepts. Exact database fields are defined in the extraction and cleaning scripts.

- `t0`: suspected-infection time zero in MIMIC-IV, anchored to microbiology culture timing after linkage to a Sepsis-3-positive SOFA time.
- `antibiotic_delay_hours`: time from t0 to first intravenous antibiotic initiation.
- `strategy`: assigned clone strategy, one of 0-3 h, 3-6 h, or 6-12 h.
- `death_30d`: 30-day all-cause mortality from t0 in MIMIC-IV.
- `age`, `sex`, `race`, `admission_category`: baseline demographic and admission covariates.
- `charlson`: Charlson comorbidity index.
- `sofa_t0`: baseline SOFA score used for adjustment around t0.
- `shock_t0`: baseline shock status derived from vasoactive-rate fields in the primary MIMIC-IV analysis.
- `ipcw`: inverse probability of censoring weight for artificial censoring.

Patient identifiers are required internally for database linkage but are not included in public aggregate outputs.
