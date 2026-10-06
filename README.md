# Network-Adjusted EB Adult Mortality — Reproducible Pipeline

Replication code for *Network-Adjusted Empirical Bayes Estimation of
Age-Specific Adult Mortality from DHS Sibling Survival Histories: Evidence
from Nigeria*. Released publicly alongside the manuscript; NDHS microdata are third-party data under the DHS Program's own
terms and are not included.

## Structure

```
R/
  00_setup.R              packages, paths, constants
  01_data_prep.R          load DHS round, visibility estimator (M_agg, M_ind)
  02_data_quality.R       Whipple/Myers age heaping, date telescoping
  03_eb_shrinkage.R       heteroscedastic SURE-EB (primary model, Table 3)
  04_bootstrap.R          Rao-Wu bootstrap propagated through EB
  05_sensitivity.R        invisibility (K), reporting accuracy (gamma),
                          internal consistency (R_alpha, B_alpha, A_alpha)
  06_life_table.R         abridged life table, partial e(15-50)
  07_benchmarks.R         WPP / GBD harmonization to 5-yr bands
  08_compare_external.R   EB vs. benchmarks, 2024 vs. 2018
  09_eb_trend_optional.R  OPTIONAL age-covariate extension (not primary)
run_all.R                 runs everything, writes outputs/
data_raw/                 place input files here (see below)
outputs/                  all CSV/RDS results written here
```

## Required input files (`data_raw/`)

| File | Source |
|---|---|
| `NGIR8BFL.DTA`, `NGSR8BFL.DTA` | Nigeria DHS 2024, IR + SR recodes |
| `NGIR7BFL.DTA`, `NGSR7BFL.DTA` | Nigeria DHS 2018, IR + SR recodes |
| `wpp_nigeria_raw.csv` | included; single-year WPP rates, ages 15-50, 2017-2024 |
| `gbd_nigeria.csv` | user-supplied IHME GBD 2021 export (all-cause, Rate, by age/sex/year) |

DHS files require registration at dhsprogram.com and are not redistributed here.

## Packages

CRAN: `haven`, `dplyr`, `tidyr`, `purrr`, `stringr`
GitHub: `siblingsurvival`, `surveybootstrap`

## Running

```r
source("run_all.R")
```

Runs both survey rounds end-to-end and writes one CSV per result object to
`outputs/` (e.g. `2024_eb.csv`, `2024_life_table.csv`, `comparison_rounds.csv`).



## What each script reproduces

| Manuscript item | Script |
|---|---|
| Table 1 (sample construction) | `01_data_prep.R` |
| Age heaping / telescoping checks | `02_data_quality.R` |
| Table 2 (M_agg vs M_ind, visibility ratio) | `01_data_prep.R` output directly |
| Table 3 (SURE-EB parameters, M_EB, 95% CI) | `03_eb_shrinkage.R` + `04_bootstrap.R` |
| Internal consistency (R_alpha, B_alpha, A_alpha) | `05_sensitivity.R` |
| Invisibility (K) / reporting-accuracy (gamma) sensitivity | `05_sensitivity.R` |
| Life table, temporary life expectancy 35e15 | `06_life_table.R` |
| WPP / GBD / 2018-vs-2024 comparison | `07_benchmarks.R`, `08_compare_external.R` |
| Age-covariate EB (reviewer-response Option B) | `09_eb_trend_optional.R` — not part of primary results |
