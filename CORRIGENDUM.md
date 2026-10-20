# Corrigendum: three coding defects in `Garden_Model.R`

**Manuscript.** Whitney, C., Luu, T. T. G., Kopton, J. & Luedeling, E. (2026) A decision-analysis framework for school garden investments in Vietnam: evaluating trade-offs for nutrition, biodiversity, and economic outcomes. *Humanities and Social Sciences Communications* 13(1), 580. https://doi.org/10.1057/s41599-026-07204-0

**Code.** This repository, `Garden_Model.R`, commit `8fa2272`; archived on Zenodo, 10.5281/zenodo.17171829 (v1.0.0, as cited in the manuscript). That archived version is left unchanged and will always reflect exactly the code used for the published results. This branch holds the corrected version; see the repository's Zenodo history for the corrected release once published.

## Summary

While re-using this model for a follow-up value-of-information analysis we found three coding defects. Running the code as published reproduces the numbers in the manuscript, so the manuscript reflects the code as it stands. Correcting the defects changes the results substantially, but not the main ranking of options:

- A passive garden still beats a STEM garden, and both still beat the no-garden use of the land, on average (private passive garden: mean NPV 1,639 → 1,417 million VND).
- Mean NPVs fall by 14% to 73% across the four decision options (public school STEM garden: 407 → 111, close to zero).
- For public schools the **median** NPV of the passive garden changes sign (516 → -165) and the probability of a positive NPV falls from 0.61 to 0.46. This is the conclusion most affected.
- The Pareto picture changes: garden size now trades economic return against biodiversity, so the "largest garden is optimal" conclusion no longer holds.
- The stand-alone health and biodiversity results are **not** changed by the corrections.

## The three defects

### Fix 1: the "no land" branch was overwritten

`stop_garden_no_land <- chance_event(1-land_access)` fed an `if` that set benefits and costs to zero for schools without land. A second `if`, on land suitability and bureaucratic barriers, then unconditionally assigned `total_benefit_public_school`, `total_cost_public_school` and the STEM equivalents in both of its branches, overwriting the first. As a result `land_access` had no effect on any NPV; it only zeroed the stand-alone environment and health outputs. The fix skips the second block once the no-land case has already applied (and also zeroes the `_inclusive` variants in the no-land block itself, which had relied on the overwrite below to be set at all).

### Fix 2: cost-zeroing assignment modified `total_cost`

```r
total_cost_public_school <- (total_cost[2:number_of_years]<-0)
total_cost_STEM_public_school <- (total_cost_STEM[2:number_of_years]<-0)
```

The intent, stated in a code comment ("costs only in year 1"), was to keep year-1 establishment costs and drop later costs when the garden is stopped. The assignment instead (a) zeroed years 2--n of `total_cost` itself, which is also used for the *private-school* results computed later, and (b) set `total_cost_public_school` to the scalar `0`, losing even the year-1 cost. The fix uses `c(total_cost[1], rep(0, n-1))` and leaves `total_cost` untouched.

### Fix 3: per-m² maintenance costs were never added

The expression `garden_maintenance_cost + ((maintaining_labor_m2 + seed_costs_m2 + fertilizer_m2 + plant_protection_m2) * size_of_garden)` was evaluated but not assigned, so these area-based costs never entered `garden_maintenance_cost`. The fix assigns it. On its own this lowers the mean NPVs of the four options by 77 to 85 million VND.

## Key numbers, published vs corrected

10,000 Monte Carlo runs, `set.seed(42)`. Million VND.

| Output | Mean, published | Mean, corrected | Change | Median, published → corrected | P(NPV > 0), published → corrected |
|---|---:|---:|---:|---|---|
| Private school, passive garden | 1,639 | 1,417 | -14% | 1,352 → 1,126 | 0.96 → 0.91 |
| Private school, STEM garden | 1,064 | 657 | -38% | 854 → 419 | 0.80 → 0.66 |
| Public school, passive garden | 902 | 545 | -40% | 516 → -165 | 0.61 → 0.46 |
| Public school, STEM garden | 407 | 111 | -73% | -138 → -249 | 0.45 → 0.33 |

## Limitations of this comparison

- The comparison is between the code as published and the corrected code on the same inputs. The inputs (expert-elicited ranges) and the model structure are not questioned; only coding defects were corrected.
- The model was checked only for the defects above. This is not a full audit.

## Reference

A fuller write-up with reproducible code, the Pareto re-analysis, and the per-variable value-of-information results is in the companion repository: https://github.com/CWWhitney/value_of_information_examples (`corrigendum_school_garden_model.Rmd`).
