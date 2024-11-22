test_that("row level classification works", {
  testdata <- random_datset(100)
  res <- classify_data(
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
  expect_true(all(grepl(
    pattern = "^input|^indicator", x = colnames(res)
  )))
  expect_true(all(c(
    "input_age", "input_sex", "input_pregnancy_status",
    "input_lactating_status", "input_ferritin", "input_iodine",
    "input_CRP", "input_AGP", "input_haemoglobin", "input_is_smoker",
    "input_altitude", "input_smokes_cigarettes_per_day", "input_pregnancyweeks",
    "input_pregnancymonths"
  ) %in% colnames(res)))
})
