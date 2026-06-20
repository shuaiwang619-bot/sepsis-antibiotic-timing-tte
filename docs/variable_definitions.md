# Variable definitions and public-release boundary

The analysis uses database identifiers internally for linkage, including MIMIC-IV patient, admission, and ICU-stay keys and eICU patient-unit-stay keys. These identifiers are not included in public outputs.

Public CSV files in `outputs_aggregate/` contain aggregate estimates, model contrasts, diagnostics, covariate-balance summaries, or table-level counts only. They should not contain one row per patient, clone, or person-period.
