
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

## Installation

You can install the development version of micronutrients from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("WorldHealthOrganization/CRANMicronutrientsSurveyAnalyser")
```

## API

There are three functions exported at the moment:

- `classify_data` to compute row level indicators
- `compute_long_format_prevalence` to compute detailed prevalence
  estimates and other summary statistics
- `compute_short_format_prevalence` to compute less-detailed prevalence
  estimates and other summary statistics

## Example

To compute row-level classifiction you can do the following:

``` r
dataset <- read.csv("some_data_set")
result <- classify_data(
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
result <- compute_short_format_prevalence(
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
#> micronutrients Coverage: 59.15%
#> R/concept-area.R: 0.00%
#> R/concept-fasting-status.R: 0.00%
#> R/concept-helpers.R: 0.00%
#> R/concept-iron-deficiency.R: 0.00%
#> R/concept-lactating-status.R: 0.00%
#> R/concept-mothers-education.R: 0.00%
#> R/concept-wealth-quintile.R: 0.00%
#> R/indicators-iron-deficiency-anaemia.R: 0.00%
#> R/indicators-ferritin.R: 0.98%
#> R/indicators-anaemia.R: 3.70%
#> R/measurements.R: 11.43%
#> R/concept-sex.R: 22.22%
#> R/utils.R: 31.82%
#> R/indicators-iodine.R: 33.33%
#> R/indicators.R: 60.10%
#> R/concept-pregnancy-status.R: 81.25%
#> R/age-groups.R: 83.67%
#> R/concepts.R: 95.00%
#> R/prevalence.R: 95.09%
#> R/classifications.R: 100.00%
```
