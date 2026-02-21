# this test suite here is non-exhaustive, but focus mostly on the cases
# where the result is positive
test_that("ida classification for unadjusted values", {
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_arithmetic_correction()),
      indicator_anaemia(),
      indicator_ida()
    ),
    age = c(10, 10),
    sex = c("m", "m"),
    ferritin = c(10, 300),
    CRP = c(5, 0),
    AGP = c(5, 0),
    haemoglobin = c(112, 112),
    altitude = c(500, 500)
  )
  expect_equal(
    as.character(res$indicator_anaemia_result),
    c(
      "Moderate anaemia",
      "Moderate anaemia"
    )
  )
  expect_equal(
    as.character(res$indicator_ferritin_unadj_result),
    c(
      "Iron deficiency in apparently healthy individuals",
      "Risk of overload in apparently healthy individuals"
    )
  )
  expect_equal(
    as.character(res$indicator_ida_unadj_result),
    c(
      "Iron deficiency anaemia in apparently healthy individuals",
      "No iron deficiency anaemia"
    )
  )
})

test_that("ida classification for 'adjustment_ferritin_rm_agp_crp'", {
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_rm_agp_crp()),
      indicator_anaemia(),
      indicator_ida()
    ),
    age = c(10, 10),
    sex = c("m", "m"),
    ferritin = c(10, 300),
    CRP = c(0, 0),
    AGP = c(0, 0),
    haemoglobin = c(112, 112),
    altitude = c(500, 500),
    malaria = c("n", "n")
  )
  expect_equal(
    as.character(res$indicator_anaemia_result),
    c(
      "Moderate anaemia",
      "Moderate anaemia"
    )
  )
  expect_equal(
    as.character(res$indicator_ferritin_adj_result),
    c(
      "Iron deficiency in apparently healthy individuals",
      "Risk of overload in apparently healthy individuals"
    )
  )
  expect_equal(
    as.character(res$indicator_ida_adj_result),
    c(
      "Iron deficiency anaemia in apparently healthy individuals",
      "No iron deficiency anaemia"
    )
  )
})

test_that("ida classification for 'adjustment_ferritin_rm_agp_crp'", {
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_cutoff()),
      indicator_anaemia(),
      indicator_ida()
    ),
    age = c(10, 10, 10),
    sex = c("m", "m", "m"),
    ferritin = c(10, 300, 10),
    CRP = c(0, 0, 6),
    AGP = c(0, 0, 6),
    haemoglobin = c(112, 112, 112),
    altitude = c(500, 500, 500),
    malaria = c("n", "n", "n")
  )
  expect_equal(
    as.character(res$indicator_anaemia_result),
    c(
      "Moderate anaemia",
      "Moderate anaemia",
      "Moderate anaemia"
    )
  )
  expect_equal(
    as.character(res$indicator_ferritin_adj_result),
    c(
      "Iron deficiency in apparently healthy individuals",
      "Risk of overload in apparently healthy individuals",
      "Iron deficiency in individuals with infection or inflammation"
    )
  )
  expect_equal(
    as.character(res$indicator_ida_adj_result),
    c(
      "Iron deficiency anaemia in apparently healthy individuals",
      "No iron deficiency anaemia",
      "Iron deficiency anaemia in individuals with infection or inflammation"
    )
  )
})

test_that("if no anaemia then no iron deficiency", {
  testdata <- random_datset(100)
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_cutoff()),
      indicator_anaemia(),
      indicator_ida()
    ),
    age = testdata$age_years,
    sex = testdata$sex,
    ferritin = testdata$ferritin_measurement,
    CRP = testdata$crp_measurement,
    AGP = testdata$agp_measurement,
    haemoglobin = testdata$haemoglobin_measurement,
    altitude = testdata$altitude
  )
  expect_true(is.factor(res$indicator_ida_adj_result))
  expect_true(is.factor(res$indicator_ida_unadj_result))
  expect_true(
    all(
      # if anaemia adj => no iron deficiency
      as.character(res$indicator_anaemia_result) != "No anaemia" |
        as.character(res$indicator_ida_adj_result) ==
          "No iron deficiency anaemia",
      na.rm = TRUE
    ) &&
      all(
        # if anaemia unadj => no iron deficiency
        as.character(res$indicator_anaemia_result) != "No anaemia" |
          as.character(res$indicator_ida_unadj_result) ==
            "No iron deficiency anaemia",
        na.rm = TRUE
      )
  )
})
