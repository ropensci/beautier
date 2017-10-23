context("create_clock_model")

test_that("multiplication works", {

  clock_model <- beastscriptr::create_clock_model(name = "relaxed_log_normal")
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

  clock_model <- beastscriptr::create_clock_model(name = "strict")
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

  clock_model <- beastscriptr::create_rln_clock_model()
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

  clock_model <- beastscriptr::create_strict_clock_model()
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

})


