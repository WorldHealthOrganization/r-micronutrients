test_that("composite indicator ida error if anaemia is not present", {
  testdata <- random_datset(100)
  expect_error(
    individual_classification(
      indicators = list(
        indicator_ferritin(adjustment_ferritin_cutoff()),
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
    ),
    "anaemia"
  )
  expect_error(
    individual_classification(
      indicators = list(
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
    ),
    "ferritin"
  )
})
