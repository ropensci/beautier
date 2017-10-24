context("create_clock_model")

test_that("use", {

  clock_model <- beastscriptr::create_clock_model(name = "relaxed_log_normal")
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

  clock_model <- beastscriptr::create_clock_model(name = "strict")
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

  clock_model <- beastscriptr::create_rln_clock_model()
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

  clock_model <- beastscriptr::create_strict_clock_model()
  testthat::expect_true(beastscriptr::is_clock_model(clock_model))

})

test_that("abuse", {

  testthat::expect_error(
    beastscriptr::create_clock_model(name = "nonsense")
  )
})

test_that("create strict clock with rate", {

  clock_model <- beastscriptr::create_clock_model(name = "strict", rate = 0.5)
  testthat::expect_equal(get_clock_model_rate(clock_model), 0.5)

  clock_model <- beastscriptr::create_strict_clock_model(rate = 0.5)
  testthat::expect_equal(get_clock_model_rate(clock_model), 0.5)

})
