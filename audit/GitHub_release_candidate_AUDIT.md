# GitHub Release Candidate Audit

Updated: 2026-06-20

Candidate path: `[local path omitted]

## Summary

- Release candidate folder: `tte_git`
- Public manifest: `audit/release_manifest.csv`
- SHA256 hashes: `audit/file_hashes_sha256.csv`
- Sensitive-data scan: `audit/sensitive_scan_report.csv`
- Fatal findings: 0
- Warning findings: 0

## Interpretation

Fatal findings must be resolved before public upload. Warnings should be reviewed manually.

Code files may legitimately contain database identifier column names as part of SQL/R logic. The stricter release check is whether public CSV/data outputs contain identifier columns or patient-level rows.

## Public-Release Boundary

This candidate intentionally excludes patient-level source data, cleaned/imputed model-ready datasets, clone/person-period tables, and individual-level IPCW weights.

## Known Upload Blockers

- A target GitHub repository is still required.
- `CITATION.cff` still contains a placeholder repository URL.
- Local `git` and GitHub CLI `gh` were not available on the Windows PATH when this candidate was prepared.

