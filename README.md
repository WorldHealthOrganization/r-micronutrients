
<!-- README.md is generated from README.Rmd. Please edit that file -->

# micronutrients

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/WorldHealthOrganization/CRANMicronutrientsSurveyAnalyser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WorldHealthOrganization/CRANMicronutrientsSurveyAnalyser/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`micronutrients` is currently an early prototype to compute
micronutrient indicators from survey data.

It is not meant for serious use yet.

## API

The following functions compute results:

- `individual_classification` to compute row level indicators
- `prevalence_long_format` to compute detailed prevalence estimates and
  other summary statistics
- `prevalence_short_format` to compute less-detailed prevalence
  estimates and other summary statistics

Indicators:

- `indicator_anaemia()`
- `indicator_ferritin(adjustment)`
- `indicator_ida(adjustment)`
- `indicator_iodine()`

Adjustments:

- `adjustment_none()`
- `adjustment_ferritin_arithmetic_correction()`
- `adjustment_ferritin_cutoff()`
- `adjustment_ferritin_regression_correction()`
- `adjustment_ferritin_rm_agp_crp()`

Depending on the indicators and adjustment methods you use you have to
input specific values, like `CRP` or `AGP`.

## Example

To compute row-level classification you can do the following:

``` r
dataset <- read.csv("some_data_set")
result <- individual_classification(
  indicators = list(
    indicator_iodine(),
    indicator_ferritin(adjustment_none()),
    indicator_ferritin(adjustment_ferritin_cutoff()),
    indicator_anaemia(),
    indicator_ida()
  ),
  age = dataset$age_years,
  sex = dataset$sex,
  pregnancy_status = dataset$pregnancy_status,
  lactating_status = dataset$lactating_status,
  ferritin = dataset$ferritin_measurement,
  iodine = dataset$iodine,
  CRP = dataset$crp_measurement,
  AGP = dataset$agp_measurement,
  haemoglobin = dataset$haemoglobin_measurement,
  is_smoker = dataset$is_smoker,
  altitude = dataset$altitude,
  smokes_cigarettes_per_day = dataset$smokes_cigarettes_per_day,
  pregnancyweeks = dataset$pregnancyweeks,
  pregnancymonths = dataset$pregnancymonths
)
```

Prevalence functions accept the same arguments and compute a summary
table.

``` r
result <- prevalence_short_format(
  indicators = list(
    indicator_iodine(),
    indicator_ferritin(adjustment_none()),
    indicator_ferritin(adjustment_ferritin_cutoff()),
    indicator_anaemia(),
    indicator_ida()
  ),
  age = testdata$age_years,
  sex = testdata$sex,
  pregnancy_status = testdata$pregnancy_status,
  lactating_status = testdata$lactating_status,
  ferritin = testdata$ferritin_measurement,
  iodine = testdata$iodine,
  CRP = testdata$crp_measurement,
  AGP = testdata$agp_measurement,
  haemoglobin = testdata$haemoglobin_measurement,
  is_smoker = testdata$is_smoker,
  altitude = testdata$altitude,
  smokes_cigarettes_per_day = testdata$smokes_cigarettes_per_day,
  pregnancyweeks = testdata$pregnancyweeks,
  pregnancymonths = testdata$pregnancymonths
)
```

## Test coverage

``` r
covr::package_coverage()
#> micronutrients Coverage: 81.73%
#> R/concept-area.R: 0.00%
#> R/concept-fasting-status.R: 0.00%
#> R/concept-helpers.R: 0.00%
#> R/concept-iron-deficiency.R: 0.00%
#> R/concept-lactating-status.R: 0.00%
#> R/concept-mothers-education.R: 0.00%
#> R/concept-wealth-quintile.R: 0.00%
#> R/measurements.R: 11.43%
#> R/concept-sex.R: 22.22%
#> R/utils.R: 29.17%
#> R/indicators-iodine.R: 33.33%
#> R/indicators-ferritin.R: 65.82%
#> R/indicators-iron-deficiency-anaemia.R: 74.64%
#> R/concept-pregnancy-status.R: 81.25%
#> R/indicators.R: 85.28%
#> R/age-groups.R: 89.61%
#> R/prevalence.R: 96.52%
#> R/adjustments-export.R: 100.00%
#> R/classifications.R: 100.00%
#> R/concepts.R: 100.00%
#> R/indicators-anaemia.R: 100.00%
#> R/indicators-export.R: 100.00%
```
