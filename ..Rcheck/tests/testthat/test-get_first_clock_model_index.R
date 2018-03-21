context("get_first_clock_model_index")

test_that("use", {

  a <- create_strict_clock_model(id = 1)
  b <- create_rln_clock_model(id = 2)
  ab <- list(a, b)

  testthat::expect_equal(beautier:::get_first_clock_model_index(a, ab), 1)
  testthat::expect_equal(beautier:::get_first_clock_model_index(b, ab), 2)
})

test_that("invalid argument types", {

  testthat::expect_error(
    beautier:::get_first_clock_model_index(
      clock_model = "nonsense",
      clock_models = list(create_strict_clock_model())
    ),
    "'clock_model' must be a clock model"
  )

  testthat::expect_error(
    beautier:::get_first_clock_model_index(
      clock_model = create_strict_clock_model(),
      clock_models = "nonsense"
    ),
    "'clock_models' must be a list of clock models"
  )

})

test_that("clock model absent", {

  a <- create_strict_clock_model(id = 1)
  b <- create_rln_clock_model(id = 2)
  aa <- list(a, a)

  testthat::expect_true(
    is.null(beautier:::get_first_clock_model_index(b, aa))
  )

})
