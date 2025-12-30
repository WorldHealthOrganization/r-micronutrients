test_that("ferritin_adjustment_arithmetic_correction warns if CRP or AGP are all NAs", {
  adj <- ferritin_adjustment_arithmetic_correction
  expect_warning(
    adj$fun(1, NA_real_, 1)
  )
  expect_warning(
    adj$fun(1, 1, NA_real_)
  )
  expect_silent(
    adj$fun(1, 1, 1)
  )
})

test_that("ferritin_adjustment_rm_agp_crp warns if CRP, AGP or malaria are all NAs", {
  adj <- ferritin_adjustment_rm_agp_crp
  expect_warning(
    adj$fun(1, NA_real_, 1, "yes")
  )
  expect_warning(
    adj$fun(1, 1, NA_real_, "yes")
  )
  expect_warning(
    adj$fun(1, 1, 1, NA_character_)
  )
  expect_silent(
    adj$fun(1, 1, 1, "yes")
  )
})

test_that("ferritin_adjustment_regression_correction warns if CRP or AGP are all NAs", {
  adj <- ferritin_adjustment_regression_correction
  expect_error(
    adj$fun(10, NA_real_, 1)
  )
  expect_error(
    adj$fun(10, 1, NA_real_)
  )
  expect_silent(
    adj$fun(1:2, c(5, 7), c(3, 7))
  )
})
