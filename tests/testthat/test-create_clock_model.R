context("create_clock_model")

test_that("use", {

  clock_model <- beautier::create_clock_model(name = "relaxed_log_normal")
  testthat::expect_true(beautier::is_clock_model(clock_model))

  clock_model <- beautier::create_clock_model(name = "strict")
  testthat::expect_true(beautier::is_clock_model(clock_model))

  clock_model <- beautier::create_rln_clock_model()
  testthat::expect_true(beautier::is_clock_model(clock_model))

  clock_model <- beautier::create_strict_clock_model()
  testthat::expect_true(beautier::is_clock_model(clock_model))

})

test_that("abuse", {

  testthat::expect_error(
    beautier::create_clock_model(name = "nonsense")
  )
})

test_that("create strict clock with rate", {

  clock_model <- beautier::create_clock_model(name = "strict", rate = 0.5)
  testthat::expect_equal(get_clock_model_rate(clock_model), 0.5)

  clock_model <- beautier::create_strict_clock_model(rate = 0.5)
  testthat::expect_equal(get_clock_model_rate(clock_model), 0.5)

})
