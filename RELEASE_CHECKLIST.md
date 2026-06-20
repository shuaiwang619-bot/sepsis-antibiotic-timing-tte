# Public Release Checklist

Use this checklist before uploading or making the repository public.

## Required Before Upload

- [ ] Confirm that the manuscript authors approve public release timing.
- [ ] Confirm whether release should be public now or private until acceptance/publication.
- [ ] Create or identify the target GitHub repository.
- [ ] Replace the placeholder `repository-code` URL in `CITATION.cff`.
- [ ] Re-run the sensitive-data scan and confirm `0 fatal / 0 warning`.
- [ ] Rebuild `audit/file_hashes_sha256.csv` after any file change.

## Files That Must Not Be Added

- patient-level MIMIC-IV or eICU files;
- cleaned or imputed analysis datasets;
- clone-level or person-period files;
- individual-level weights;
- files containing `subject_id`, `hadm_id`, `stay_id`, or `patientunitstayid` as released data columns;
- submission-system files, cover letters, reviewer notes, author information forms, or personal contact spreadsheets;
- database credentials, local environment files, or API keys.

## Suggested Upload Contents

- `README.md`
- `LICENSE`
- `CITATION.cff`
- `.gitignore`
- `code/`
- `docs/`
- `examples/`
- `figures/`
- `outputs_aggregate/`
- `audit/`

## Post-Upload Checks

- [ ] Open the GitHub repository in a browser and confirm the README renders correctly.
- [ ] Confirm no hidden large files or restricted data were uploaded.
- [ ] Confirm figures open from GitHub.
- [ ] Confirm `audit/GitHub_release_candidate_AUDIT.md` and `audit/sensitive_scan_report.csv` are present.
- [ ] Copy the repository URL into the manuscript availability statement only if the timing matches journal policy and author preference.
