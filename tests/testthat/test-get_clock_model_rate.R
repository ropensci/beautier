context("get_clock_model_rate")

test_that("use", {

  testthat::expect_equal(
    get_clock_model_rate(create_strict_clock_model()),
    get_default_clock_model_rate()
  )

})
