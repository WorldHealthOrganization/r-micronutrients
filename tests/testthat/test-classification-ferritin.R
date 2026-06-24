test_that("adequate iron stores differentiate between health and inflammation", {
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_cutoff())
    ),
    age = c(1, 1),
    sex = c("m", "m"),
    ferritin = c(13, 50),
    CRP = c(0, 6),
    AGP = c(0, 6)
  )
  expect_equal(
    as.character(res$indicator_ferritin_adj_result),
    c(
      "Adequate iron stores in apparently healthy individuals",
      "Adequate iron stores in individuals with infection or inflammation"
    )
  )
  expect_equal(
    as.character(res$indicator_ferritin_unadj_result),
    c(
      "Adequate iron stores in apparently healthy individuals",
      "Adequate iron stores in apparently healthy individuals"
    )
  )

  # inflammation is only considered when using the cutoff adjustment
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_arithmetic_correction())
    ),
    age = c(1, 1),
    sex = c("m", "m"),
    ferritin = c(13, 50),
    CRP = c(0, 6),
    AGP = c(0, 6)
  )
  expect_equal(
    as.character(res$indicator_ferritin_adj_result),
    c(
      "Adequate iron stores in apparently healthy individuals",
      "Adequate iron stores in apparently healthy individuals"
    )
  )
  expect_equal(
    as.character(res$indicator_ferritin_unadj_result),
    c(
      "Adequate iron stores in apparently healthy individuals",
      "Adequate iron stores in apparently healthy individuals"
    )
  )
})

test_that("regression correction handles invalid values", {
  res <- individual_classification(
    indicators = list(
      indicator_ferritin(adjustment_ferritin_regression_correction())
    ),
    age = c(1, 2, 3),
    sex = c("m", "m", "m"),
    ferritin = c(13, 12, 11),
    CRP = c(1, 1, 1),
    AGP = c(1, 1, NA_real_)
  )
  expect_equal(
    is.na(res$indicator_ferritin_adj_result),
    c(FALSE, FALSE, TRUE)
  )
})
