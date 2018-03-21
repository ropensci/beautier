context("get_clock_model_name")

test_that("use", {

  testthat::expect_equal(
    get_clock_model_name(create_strict_clock_model()),
    "StrictClock"
  )

  testthat::expect_equal(
    get_clock_model_name(create_rln_clock_model()),
    "RelaxedClock"
  )

})
