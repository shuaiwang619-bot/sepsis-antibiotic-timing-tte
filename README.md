# Sepsis Antibiotic Timing Target Trial Emulation

This repository is the public reproducibility companion for:

**Early versus later antibiotic initiation and 30-day mortality among critically ill adults with sepsis: a target trial emulation**

The primary analysis uses MIMIC-IV v3.1 to emulate antibiotic-initiation strategies after suspected-infection time zero. The eICU Collaborative Research Database v2.0 is used as an ICU-admission-anchored directional exploration, not as a strict same-definition external validation.

## What Is Included

- `code/`: SQL, R, Python, and JavaScript scripts for extraction, cleaning, multiple imputation, clone-censor-weight analysis, IPCW diagnostics, sensitivity analyses, eICU directional exploration, and table/figure generation.
- `docs/`: data access notes, run order, target-trial protocol summary, variable definitions, and repository-publication notes.
- `outputs_aggregate/`: aggregate result tables and diagnostic summaries only.
- `figures/`: browser-friendly public figure assets generated from aggregate outputs or figure scripts.
- `examples/`: synthetic minimal example data only.
- `audit/`: release manifest, sensitive-data scan report, and file hashes for this release candidate.

## What Is Not Included

This repository intentionally excludes:

- patient-level MIMIC-IV or eICU data;
- cleaned or imputed patient-level analysis datasets;
- clone-level or person-period tables;
- individual-level IPCW weights;
- database credentials, local paths, or private submission files.

## Data Access

MIMIC-IV and eICU patient-level data are not redistributed here. Users must obtain credentialed access through PhysioNet and run the extraction scripts in their own approved environment.

See `docs/data_access.md` for details.

## Reproducibility Scope

The repository supports transparent review of the analysis logic and reproduction by credentialed users who can access the source databases. The aggregate outputs are included to document the reported estimates and diagnostics without redistributing restricted patient-level data.

Suggested run order is provided in `docs/run_order.md`.

## Public Release Status

This folder is a local GitHub release candidate. Before public release, confirm:

1. the associated manuscript has been accepted or the authors approve public pre-acceptance release;
2. the GitHub repository URL has replaced the placeholder in `CITATION.cff`;
3. the final sensitive-data scan remains clear;
4. no patient-level files have been added.

See `RELEASE_CHECKLIST.md`.

## Citation

If you use this code, please cite the associated article once available. A `CITATION.cff` file is included and should be updated with the final DOI after publication.

## License

Code is released under the MIT License. This license applies to the code in this repository only and does not grant permission to redistribute MIMIC-IV, eICU, or any derived patient-level data.
