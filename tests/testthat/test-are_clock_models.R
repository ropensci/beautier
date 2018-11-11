context("are_clock_models")

test_that("use", {

  testthat::expect_true(are_clock_models(create_strict_clock_model()))

  testthat::expect_false(are_clock_models("nonsense"))
  testthat::expect_false(are_clock_models(rep("nonsense", 2)))

  testthat::expect_false(are_clock_models(NA))
  testthat::expect_false(are_clock_models(NULL))

})
