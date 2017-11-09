context("initialize_clock_models")

test_that("initialize RLN clock model", {

  before <- list(create_rln_clock_model())
  testit::assert(!beautier:::are_initialized_clock_models(before))
  after <- beautier:::initialize_clock_models(before)
  testthat::expect_true(beautier:::are_initialized_clock_models(after))

})
