# eICU formal_v1 workflow

本目录用于放置 eICU directional external validation 的正式生产流程。

定位：ICU-admission-anchored directional external validation。eICU 不作为 MIMIC suspected infection / culture t0 的同定义 external replication。

## Directory Rules

- `R代码/`: 只放正式流程脚本，编号和 MIMIC formal_v1 对齐。
- `outputs/`: 放正式流程生成的宽表、长表、模型、Table 1 和报告。
- `audit_from_validation/`: 放旧 `eICU验证` 目录里的定义审计、t0 审计、协变量覆盖审计。这里是证据来源，不是生产流程入口。
- `protocol/`: 放锁定协议和后续版本化方案。
- `docs/`: 放流程清单、变量字典、实现说明。

## MIMIC-Aligned Flow

| Step | Stage | MIMIC counterpart | eICU script | Purpose |
|---:|---|---|---|---|
| 01 | wide table source | `01_abx_tte_full_pipeline_formal_v1_20260612.sql` | `01_eicu_formal_v1_full_pipeline_source.R` | Build locked source components: cohort, exposure, outcome, covariate source extracts. |
| 02 | wide table merge | `02_abx_tte_merge_analysis_main_formal_v1_20260612.sql` | `02_eicu_formal_v1_merge_analysis_main.R` | Merge source components into one-row-per-`patientunitstayid` analysis main table. |
| 03 | wide table categorical | `03_formal_v1_analysis_main_categorical_encoding.R` | `03_eicu_formal_v1_analysis_main_categorical_encoding.R` | Encode checked categorical variables and create role manifest. |
| 04 | wide table cleaning | `04_formal_v1_analysis_main_extreme_value_cleaning.R` | `04_eicu_formal_v1_analysis_main_sentinel_extreme_cleaning.R` | Preserve raw columns, create clean columns, convert APACHE `-1` sentinel and implausible values to missing. |
| 05 | wide table imputation | `05_formal_v1_analysis_main_mice_imputation.R` | `05_eicu_formal_v1_analysis_main_mice_imputation.R` | Create model-ready imputed wide table if needed. |
| 06 | long table QC | `06_formal_v1_long_table_pre_ccw_qc.R` | `06_eicu_formal_v1_long_table_pre_ccw_qc.R` | Create/audit long table before clone-censor weighting. |
| 06b | long table cleaning | `06b_formal_v1_long_table_type_extreme_fill.R` | `06b_eicu_formal_v1_long_table_type_extreme_fill.R` | Clean long-table types, extremes, and fill rules. |
| 07 | CCW diagnostics | `07_formal_v1_ccw_clone_censor_diagnostics.R` | `07_eicu_formal_v1_ccw_clone_censor_diagnostics.R` | Clone-censor diagnostics. |
| 08 | CCW weights | `08_formal_v1_ccw_ipcw_weights.R` | `08_eicu_formal_v1_ccw_ipcw_weights.R` | IPCW estimation and diagnostics. |
| 09 | person-period | `09_formal_v1_ccw_30d_person_period.R` | `09_eicu_formal_v1_ccw_30d_person_period.R` | Person-period table for 30-day analysis. |
| 10 | main model | `10_formal_v1_ccw_pooled_logistic_main.R` | `10_eicu_formal_v1_ccw_pooled_logistic_main.R` | Main pooled logistic model. |
| 11 | balance | `11_formal_v1_ccw_covariate_balance.R` | `11_eicu_formal_v1_ccw_covariate_balance.R` | Covariate balance and SMD diagnostics. |
| 12 | sensitivity | `12_formal_v1_ccw_pooled_logistic_coarse_time.R` | `12_eicu_formal_v1_ccw_pooled_logistic_coarse_time.R` | Coarse-time pooled logistic sensitivity. |
| 13 | sensitivity | `13_formal_v1_ccw_weighted_cox_sensitivity.R` | `13_eicu_formal_v1_ccw_weighted_cox_sensitivity.R` | Weighted Cox sensitivity. |
| 14 | sensitivity | `14_formal_v1_ccw_pooled_logistic_truncated_weights.R` | `14_eicu_formal_v1_ccw_pooled_logistic_truncated_weights.R` | Truncated-weight sensitivity. |
| 15 | positivity | `15_formal_v1_ccw_positivity_overlap.R` | `15_eicu_formal_v1_ccw_positivity_overlap.R` | Positivity and overlap diagnostics. |
| 16 | Table 1 | `16_formal_v1_table1_baseline_characteristics.R` | `16_eicu_formal_v1_table1_baseline_characteristics.R` | Main Table 1 from model-ready wide table. |
| 17 | results | `17_formal_v1_main_results_tables_forest.R` | `17_eicu_formal_v1_main_results_tables_forest.R` | Model result tables and forest plot. |
| 18 | Table 1 sensitivity | `18_formal_v1_table1_original_analyzable_baseline.R` | `18_eicu_formal_v1_table1_original_analyzable_baseline.R` | Original analyzable baseline Table 1 sensitivity. |

## Locked eICU Definition

- Cohort: strict explicit sepsis label.
- Population: adult ICU stays, first qualifying stay per `uniquepid`.
- t0: ICU admission offset 0.
- Antibiotics: systemic candidate antibiotics after excluding non-administration, PO, and local-route records.
- Outcome: 30-day in-hospital mortality.
- Main first-pass priority: wide table, Table 1, simplified logistic/Cox. Long table and CCW/IPCW are second-stage.

## Current Implementation Status

The folder is scaffolded to match MIMIC formal_v1. The first implementation target is steps 01-05, i.e. the eICU formal wide table. Scripts marked as scaffold intentionally stop when run until implemented.
