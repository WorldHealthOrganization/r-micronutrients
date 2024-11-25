test_that("long prevalence", {
  testdata <- random_datset(100)
  res <- prevalence_long_format(
    indicators = list(
      indicator_iodine(),
      indicator_ferritin(),
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
  expect_true(is.data.frame(res))
})

test_that("short prevalence", {
  testdata <- random_datset(100)
  res <- prevalence_short_format(
    indicators = list(
      indicator_iodine(),
      indicator_ferritin(),
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
  expect_true(is.data.frame(res))
})
