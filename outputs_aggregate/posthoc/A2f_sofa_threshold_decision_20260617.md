# A2f SOFA subgroup threshold decision

Date: 2026-06-17

Purpose: define the baseline SOFA threshold before running the post-hoc SOFA subgroup CCW analysis.

Decision: use imputed baseline SOFA >= 6 as the high-severity subgroup and imputed baseline SOFA < 6 as the low-severity subgroup.

Rationale:
- In the eligible CCW sample, baseline SOFA had median 2, IQR approximately 0 to 4, P90 = 6, and P95 = 7 across imputations.
- SOFA >= 6 identifies a clinically more severe subgroup while retaining enough patients and events for the three antibiotic-initiation windows.
- Candidate thresholds >=7 and >=8 produced smaller high-severity windows, with >=8 approaching the event-count stability boundary in the 3-6 h arm.

Analysis discipline:
- The threshold was chosen after inspecting the baseline SOFA distribution and before running any SOFA-stratified outcome models.
- The analysis remains post-hoc exploratory and should be described as such in the manuscript.
- Subgroup membership is assigned within each imputed dataset using imputed baseline SOFA, consistent with the main model.
