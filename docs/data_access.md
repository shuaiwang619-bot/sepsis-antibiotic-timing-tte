# Data access

This project uses MIMIC-IV v3.1 and the eICU Collaborative Research Database v2.0. These databases require credentialed access through PhysioNet and data use agreements. Patient-level data and derived patient-level intermediate files cannot be redistributed.

The scripts in this repository are provided to document and reproduce the analysis in an approved local environment after database access is obtained.

## Required Databases

- MIMIC-IV v3.1: primary target-trial emulation.
- eICU Collaborative Research Database v2.0: directional exploration anchored at ICU admission.

## Redistribution Boundary

Do not upload or redistribute source tables, extracted patient-level cohorts, cleaned model-ready datasets, imputed datasets, clone/person-period tables, or individual-level IPCW weights.

The public folder includes aggregate outputs only. See `reproducibility_scope.md` for the release boundary.
