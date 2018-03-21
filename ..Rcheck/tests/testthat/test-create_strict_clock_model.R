context("create_strict_clock_model")

test_that("use", {

  testthat::expect_true(
    is_strict_clock_model(
      create_strict_clock_model()
    )
  )
})

test_that("abuse", {

  testthat::expect_error(
    create_strict_clock_model(clock_rate_param = NA),
    "'clock_rate_param' must be a clock rate parameter"
  )

  testthat::expect_error(
    create_strict_clock_model(clock_rate_distr = NA),
    "'clock_rate_distr' must be a distribution"
  )

})
