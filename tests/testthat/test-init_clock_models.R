context("init_clock_models")

test_that("initialize RLN clock model", {

  id <- "a"
  before <- list(create_rln_clock_model())
  testit::assert(!beautier:::are_init_clock_models(before))
  after <- beautier:::init_clock_models(before, ids = id)
  testthat::expect_true(beautier:::are_init_clock_models(after))

})

test_that("initialize strict clock model", {

  id <- "a"
  before <- list(create_strict_clock_model())
  testit::assert(!beautier:::are_init_clock_models(before))
  after <- beautier:::init_clock_models(before, ids = id)
  testthat::expect_true(beautier:::are_init_clock_models(after))
})
