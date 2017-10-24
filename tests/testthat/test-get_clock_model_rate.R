context("get_clock_model_rate")

test_that("use", {

  testthat::expect_equal(
    get_clock_model_rate(create_strict_clock_model()),
    get_default_clock_model_rate()
  )

  testthat::expect_equal(
    get_clock_model_rate(create_strict_clock_model(rate = 0.5)),
    0.5
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_clock_model_rate("nonsense")
  )

})
