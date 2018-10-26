context("create_strict_clock_model")

test_that("use", {

  expect_true(
    is_strict_clock_model(
      create_strict_clock_model()
    )
  )

  expect_true(
    is_strict_clock_model(
      create_strict_clock_model(
        clock_rate_param = create_clock_rate_param(value = 3.14)
      )
    )
  )

  # Simplified interface
  expect_true(
    is_strict_clock_model(
      create_strict_clock_model(
        clock_rate_param = 3.14
      )
    )
  )

})

test_that("abuse", {

  expect_error(
    create_strict_clock_model(clock_rate_param = NA),
    "'clock_rate_param' must be a clock rate parameter"
  )

  expect_error(
    create_strict_clock_model(clock_rate_distr = NA),
    "'clock_rate_distr' must be a distribution"
  )

})
