test_that("row level classification works", {
  testdata <- random_datset(100)
  res <- individual_classification(
    indicators = list(
      indicator_iodine(),
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
  expect_true(is.data.frame(res))
  expect_true(all(grepl(
    pattern = "^input|^indicator",
    x = colnames(res)
  )))
  # we expect a specific column order
  expect_equal(
    colnames(res),
    c(
      "input_age_years",
      "input_age_months",
      "input_sex",
      "input_pregnancy_status",
      "input_pregnancyweeks",
      "input_pregnancymonths",
      "input_lactating_status",
      "input_is_smoker",
      "input_smokes_cigarettes_per_day",
      "input_altitude",
      "input_iodine",
      "input_haemoglobin",
      "input_ferritin",
      "input_CRP",
      "input_AGP",
      "indicator_iodine_result",
      "indicator_iodine_input_value",
      "indicator_ferritin_unadj_result",
      "indicator_ferritin_unadj_input_value",
      "indicator_ferritin_adj_result",
      "indicator_ferritin_adj_input_value",
      "indicator_ferritin_adj_adjustment_method",
      "indicator_anaemia_result",
      "indicator_anaemia_input_value",
      "indicator_ida_unadj_result",
      "indicator_ida_adj_result"
    )
  )
  expect_true(all(res$indicator_ferritin_adj_adjustment_method == "cutoff"))
})

test_that("optional values have their prototype values", {
  testdata <- random_datset(100)
  res <- individual_classification(
    indicators = list(
      indicator_iodine(),
      indicator_ferritin(adjustment_ferritin_cutoff()),
      indicator_anaemia(),
      indicator_ida()
    ),
    age = testdata$age_years,
    sex = testdata$sex,
    ferritin = testdata$ferritin_measurement,
    haemoglobin = testdata$haemoglobin_measurement,
    iodine = testdata$iodine,
    altitude = testdata$altitude
  )
  expect_true(is.data.frame(res))
  expect_true(all(grepl(
    pattern = "^input|^indicator",
    x = colnames(res)
  )))
  expect_true(all(
    c(
      "input_sex",
      "input_ferritin",
      "input_iodine",
      "input_haemoglobin",
      "input_altitude"
    ) %in%
      colnames(res)
  ))
})

test_that("user is warned about missing concepts", {
  expect_error(
    individual_classification(
      indicators = list(
        indicator_iodine(),
        indicator_ferritin(adjustment_ferritin_cutoff()),
        indicator_anaemia(),
        indicator_ida()
      ),
      sex = "1",
      age = 1
    )
  )
})

test_that("user is warned about malformed input", {
  expect_error(
    individual_classification(
      indicators = list(
        indicator_iodine(),
        indicator_ferritin(adjustment_ferritin_cutoff()),
        indicator_anaemia(),
        indicator_ida()
      ),
      sex = "hello",
      age = 1
    ),
    regexp = "sex"
  )
})

test_that("all adjustment methods do not create errors", {
  adjustments <- list(
    adjustment_ferritin_cutoff(),
    adjustment_ferritin_rm_agp_crp(),
    adjustment_ferritin_arithmetic_correction(),
    adjustment_ferritin_regression_correction()
  )
  testdata <- random_datset(100)
  for (x in adjustments) {
    res <- individual_classification(
      indicators = list(
        indicator_ferritin(x)
      ),
      age = testdata$age_years,
      sex = testdata$sex,
      pregnancy_status = testdata$pregnancy_status,
      lactating_status = testdata$lactating_status,
      ferritin = testdata$ferritin_measurement,
      iodine = testdata$iodine,
      CRP = testdata$crp_measurement,
      AGP = testdata$agp_measurement,
      malaria = testdata$malaria,
      haemoglobin = testdata$haemoglobin_measurement,
      is_smoker = testdata$is_smoker,
      altitude = testdata$altitude,
      smokes_cigarettes_per_day = testdata$smokes_cigarettes_per_day,
      pregnancyweeks = testdata$pregnancyweeks,
      pregnancymonths = testdata$pregnancymonths
    )
    expect_true(is.data.frame(res), label = format(x))
  }
})

test_that("all inputs need to be of equal length", {
  expect_error(
    individual_classification(
      indicators = list(
        indicator_ferritin()
      ),
      age = c(1, 2),
      sex = c(1, 2, 1)
    )
  )
})

test_that("you can use lubridate to define the age", {
  testdata <- random_datset(100)
  age <- lubridate::duration(1:100, "months")
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_cutoff())
    ),
    age = age,
    sex = testdata$sex,
    ferritin = testdata$ferritin_measurement,
    haemoglobin = testdata$haemoglobin_measurement,
    pregnancy_status = testdata$pregnancy_status,
    CRP = testdata$crp_measurement,
    AGP = testdata$agp_measurement
  )
  expect_true(is.data.frame(res))
  expect_equal(res$input_age_years, as.numeric(age, "years"))
  expect_equal(res$input_age_months, as.numeric(age, "months"))
})

test_that("it warns if `is_smoker` is NULL but `smokes_cigarattes_per_day` is not", {
  testdata <- random_datset(100)
  expect_warning(
    individual_classification(
      indicators = list(
        indicator_anaemia()
      ),
      age = testdata$age_years,
      sex = testdata$sex,
      iodine = testdata$iodine,
      haemoglobin = testdata$haemoglobin_measurement,
      altitude = testdata$altitude,
      smokes_cigarettes_per_day = testdata$smokes_cigarettes_per_day
    ),
    regexp = "is_smoker"
  )
})

test_that("it warns if `pregnancy_status` is NULL but other preganancy related variables have values", {
  testdata <- random_datset(100)
  expect_warning(
    expect_warning(
      individual_classification(
        indicators = list(
          indicator_anaemia()
        ),
        age = testdata$age_years,
        sex = testdata$sex,
        iodine = testdata$iodine,
        haemoglobin = testdata$haemoglobin_measurement,
        altitude = testdata$altitude,
        pregnancyweeks = testdata$pregnancyweeks,
        pregnancymonths = testdata$pregnancymonths
      ),
      regexp = "pregnancyweeks"
    ),
    regexp = "pregnancymonths"
  )
})

test_that("age is in years and months in the output", {
  testdata <- random_datset(100)
  res <- individual_classification(
    indicators = list(
      indicator_anaemia()
    ),
    age = testdata$age_years,
    sex = testdata$sex,
    ferritin = testdata$ferritin_measurement,
    haemoglobin = testdata$haemoglobin_measurement,
    iodine = testdata$iodine,
    altitude = testdata$altitude
  )
  expect_contains(colnames(res), c("input_age_years", "input_age_months"))
  expect_null(res[["input_age"]])
  expect_true(all(res$input_age_months >= res$input_age_years))
})
