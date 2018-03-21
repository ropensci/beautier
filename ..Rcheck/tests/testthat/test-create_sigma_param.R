context("create_sigma_param")

test_that("use", {

  testthat::expect_silent(
    create_sigma_param()
  )

})

test_that("abuse", {

  testthat::expect_error(
    create_sigma_param(value = 0.0),
    "'value' must be non-zero and positive"
  )

  testthat::expect_error(
    create_sigma_param(value = -123.456),
    "'value' must be non-zero and positive"
  )

})
