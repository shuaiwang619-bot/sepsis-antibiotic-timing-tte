# Reproducibility Scope

This repository is designed to make the analysis auditable while respecting the data-use restrictions of MIMIC-IV and eICU.

## Fully Public Components

- analysis code;
- SQL extraction logic;
- aggregate result tables;
- aggregate diagnostics;
- figure-generation scripts and public figure assets;
- synthetic example input.

## Restricted Components

The following components are required for a full re-run but are not included:

- source MIMIC-IV and eICU tables;
- patient-level extracted cohorts;
- cleaned and imputed model-ready datasets;
- clone/person-period datasets;
- individual-level IPCW weights.

Credentialed users can regenerate restricted intermediate files locally by following `docs/run_order.md`.

## Interpretation of Aggregate Outputs

The aggregate outputs document the estimates, weights, balance diagnostics, and post-hoc exploratory analyses reported in the manuscript. They are included for transparency and spot-checking; they are not a substitute for a full database-level re-run by credentialed users.
