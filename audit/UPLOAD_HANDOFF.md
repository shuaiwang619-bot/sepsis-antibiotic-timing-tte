# GitHub Upload Handoff

Updated: 2026-06-20

## Current Status

The local release candidate is ready for GitHub upload.

- Candidate folder: `tte_git`
- Release zip: stored outside this candidate folder as `tte_git_release_candidate_20260620.zip`
- Sensitive-data scan: 0 findings
- Public manifest: `audit/release_manifest.csv`
- SHA256 hashes: `audit/file_hashes_sha256.csv`

## Remaining Blocker

A target GitHub repository is required before upload.

The connected GitHub account currently shows no accessible repositories, and no repository-creation tool is exposed in this Codex session. Local `git` and `gh` are also unavailable on the Windows PATH.

## Minimal Next Step

Create an empty repository, for example:

`sepsis-antibiotic-timing-tte`

Then replace the placeholder URL in `CITATION.cff` and upload the full contents of `tte_git`.

## Recommended Visibility

Keep the repository private until author approval, acceptance, publication, or the journal's preferred release timing.
