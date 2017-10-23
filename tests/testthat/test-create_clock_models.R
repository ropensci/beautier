context("create_clock_models")

test_that("all clock_models must be recognized as such", {

  clock_models <- beastscriptr::create_clock_models()
  testthat::expect_true(length(clock_models) > 1)
  for (clock_model in clock_models) {
    testthat::expect_true(beastscriptr::is_clock_model(clock_model))
  }

})
