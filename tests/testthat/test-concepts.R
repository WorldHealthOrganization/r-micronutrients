test_that("concepts_from_args: some concepts have default values if not supplied", {
  observed <- concepts_from_args(
    age = 1:5,
    sex = c(1, 2, 2, 1, 1)
  )
  rep <- \(x) rep.int(x, lengths(observed$values)[[1L]])
  observed_names <- names(observed$values)
  expect_contains(observed_names, "age")
  expect_contains(observed_names, "sex")
  expect_contains(observed$non_nulls, c("age", "sex"))
  optional_concepts <- c(
    "is_smoker",
    "smokes_cigarettes_per_day",
    "pregnancy_status",
    "pregnancyweeks",
    "pregnancymonths",
    "lactating_status",
    "CRP",
    "AGP",
    "malaria"
  )
  for (concept_key in optional_concepts) {
    expect_equal(
      observed$values[[!!concept_key]],
      rep(concepts[[!!concept_key]]$prototype)
    )
  }
})

test_that("area is properly encoded", {
  x <- as_area(c("1", "2", "urban", "rural", NA_character_))
  expect_equal(
    x,
    factor(c("Urban", "Rural", "Urban", "Rural", NA_character_), levels = area_levels)
  )
})
