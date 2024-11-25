
<!-- README.md is generated from README.Rmd. Please edit that file -->

# micronutrients

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/WorldHealthOrganization/CRANMicronutrientsSurveyAnalyser/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WorldHealthOrganization/CRANMicronutrientsSurveyAnalyser/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`micronutrients` is currently an early prototype to compute
micronutrient indicators from survey data. It is based on the code from
the [Micronutrient Survey Analyzer
tool](https://github.com/WorldHealthOrganization/micronutrients-survey-analyzer-dev/tree/dev).

It is not meant for serious use yet. The goal is to build the foundation
to turn the logic into a package once the shiny app is mature enough.

## Installation

You can install the development version of micronutrients from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("WorldHealthOrganization/CRANMicronutrientsSurveyAnalyser")
```

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
- `adjustment_ferritin_implausible()`
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
    indicator_ferritin(adjustment_ferritin_implausible()),
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
    indicator_ferritin(adjustment_ferritin_implausible()),
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
#> micronutrients Coverage: 80.96%
#> R/concept-area.R: 0.00%
#> R/concept-fasting-status.R: 0.00%
#> R/concept-helpers.R: 0.00%
#> R/concept-iron-deficiency.R: 0.00%
#> R/concept-lactating-status.R: 0.00%
#> R/concept-mothers-education.R: 0.00%
#> R/concept-wealth-quintile.R: 0.00%
#> R/indicators-anaemia.R: 3.70%
#> R/measurements.R: 11.43%
#> R/concept-sex.R: 22.22%
#> R/utils.R: 31.82%
#> R/indicators-iodine.R: 33.33%
#> R/indicators-ferritin.R: 66.99%
#> R/concept-pregnancy-status.R: 81.25%
#> R/age-groups.R: 83.67%
#> R/indicators.R: 85.31%
#> R/prevalence.R: 95.18%
#> R/indicators-iron-deficiency-anaemia.R: 97.53%
#> R/adjustments-export.R: 100.00%
#> R/classifications.R: 100.00%
#> R/concepts.R: 100.00%
#> R/indicators-export.R: 100.00%
```
