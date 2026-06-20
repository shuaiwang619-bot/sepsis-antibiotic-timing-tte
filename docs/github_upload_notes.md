# GitHub Upload Notes

This folder is prepared as a release candidate for a public reproducibility repository.

## Current Local Limitation

At the time this candidate was prepared, local `git` and GitHub CLI `gh` were not available on the Windows PATH. The connected GitHub account also did not expose an existing target repository. Therefore, this folder has not yet been committed or uploaded.

## Minimal Path to Publish

1. Create an empty GitHub repository, for example `sepsis-antibiotic-timing-tte`.
2. Grant the uploading account write access.
3. Replace the placeholder URL in `CITATION.cff`.
4. Re-run the release audit.
5. Upload the full contents of this folder.

## Recommended Repository Visibility

Use a private repository until the authors decide the public-release timing. Make it public after acceptance, publication, or explicit author approval.

## Suggested Commit Message

`Initial public reproducibility release candidate`

## Repository Description

`Code and aggregate outputs for a target trial emulation of antibiotic initiation timing and 30-day mortality in ICU sepsis.`
