# Target-trial protocol summary

Eligibility: adult first eligible ICU stays with Sepsis-3/t0 definition and available 30-day mortality status, excluding pre-t0 intravenous antibiotic users.

Strategies: intravenous antibiotic initiation within 0-3 h, 3-6 h, and 6-12 h after suspected-infection/t0.

Assignment emulation: each eligible patient is cloned into all strategies at t0.

Follow-up: from t0 to 30-day death, administrative end of follow-up, or artificial censoring.

Analysis: clone-censor-weight target trial emulation using inverse probability of censoring weights and covariate-adjusted discrete-time pooled logistic regression.

Estimand: per-protocol strategy contrasts among the initiation windows.
