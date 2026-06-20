# A4 lactate sensitivity feasibility note

Date: 2026-06-17

## Purpose

This note documents why a lactate-adjusted CCW/IPCW sensitivity analysis was not run blindly during the post-hoc Critical Care enhancement work.

## Data availability

The MICE-completed wide analysis table does not contain a baseline lactate variable.

The time-varying covariate table contains lactate-related columns:

- `lactate_tvcov`
- `lactate_tvcov_clean`
- `lactate_tvcov_fill`
- `lactate_tvcov_missing_model`
- `lactate_tvcov_extreme_or_invalid`

At `t_hour_clean == 0` in imputation 1, raw lactate was available for 8,855 of 24,735 rows, but raw values had implausible tails (median 2.8, Q3 167, maximum 19,780), consistent with broad SQL capture using `ILIKE '%LACTATE%'` and likely inclusion of non-blood-lactate assays such as lactate dehydrogenase. After cleaning, 6,382 of 24,735 rows had usable `lactate_tvcov_clean`, with median 2.1 and IQR 1.5-3.0.

## Main-analysis cohort missingness pattern

Among eligible CCW roles (`analyzable` and `censor_noinit`) across imputations 1-5, usable clean t0 lactate was highly differential by observed antibiotic timing:

| CCW role | observed window | clean t0 lactate available |
|---|---:|---:|
| analyzable | 0-3 h | 34.2% |
| analyzable | 3-6 h | 13.3% |
| analyzable | 6-12 h | 8.9% |
| analyzable | >=12 h | 10.5% |
| censor_noinit | no IV in 72 h after t0 | 12.5% |

## Interpretation

A complete-case lactate sensitivity analysis would restrict the analysis to a highly selected tested-lactate subgroup and would likely change the target population. A multiple-imputation lactate sensitivity would require a new imputation specification for a selectively measured, plausibly MNAR severity marker, and should not be added as a quick post-hoc model.

## Recommendation

Do not include a lactate-adjusted sensitivity analysis in the current manuscript unless a dedicated lactate variable audit and imputation plan is built. The manuscript should instead disclose that baseline lactate was not included because of selective measurement and high missingness, and that residual severity confounding cannot be excluded.
