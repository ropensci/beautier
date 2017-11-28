context("create_clock_model")

test_that("use", {

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
