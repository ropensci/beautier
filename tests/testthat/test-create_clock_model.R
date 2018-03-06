context("create_clock_model")

test_that("use", {

  # English
  clock_model <- beautier::create_rln_clock_model()
  testthat::expect_true(beautier:::is_rln_clock_model(clock_model))

  clock_model <- beautier::create_strict_clock_model()
  testthat::expect_true(beautier:::is_strict_clock_model(clock_model))

  # Search-tree friendly
  clock_model <- beautier::create_clock_model_rln()
  testthat::expect_true(beautier:::is_rln_clock_model(clock_model))

  clock_model <- beautier::create_clock_model_strict()
  testthat::expect_true(beautier:::is_strict_clock_model(clock_model))

})

test_that("abuse", {

  testthat::expect_error(
    beautier::create_clock_model(name = "nonsense")
  )
})
