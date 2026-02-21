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
