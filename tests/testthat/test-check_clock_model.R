test_that("use", {
  expect_silent(check_clock_model(create_strict_clock_model()))
  expect_silent(check_clock_model(create_rln_clock_model()))
})

test_that("abuse, general clock model", {

  # Must be one clock model
  expect_error(
    check_clock_model(
      list(
        create_strict_clock_model(),
        create_strict_clock_model()
      )
    ),
    "'id' must be an element of 'clock_model'"
  )

  # Must be a clock model
  expect_error(
    check_clock_model("nonsense"),
    "'id' must be an element of 'clock_model'"
  )
  expect_error(
    check_clock_model(NULL),
    "'id' must be an element of 'clock_model'"
  )
  expect_error(
    check_clock_model(NA),
    "'id' must be an element of 'clock_model'"
  )

  # Invalid clock name
  clock_model <- create_strict_clock_model()
  clock_model$name <- "nonsense"
  expect_error(
    check_clock_model(clock_model),
    "'clock_model\\$name' must be one of the clock model names"
  )

  # Invalid rate_scaler_factor
  clock_model <- create_strict_clock_model()
  clock_model$rate_scaler_factor <- "nonsense"
  expect_error(
    check_clock_model(clock_model),
    "'rate_scaler_factor' must be a number or a string that can be converted to a number or an empty string" # nolint
  )

})

test_that("abuse, rln clock model", {

  rln_clock_model <- create_rln_clock_model()
  rln_clock_model$ucldstdev_distr <- "nonsense"
  expect_error(
    check_clock_model(rln_clock_model),
    "'clock_model\\$ucldstdev_distr' must be a distribution"
  )

  rln_clock_model <- create_rln_clock_model()
  rln_clock_model$mean_rate_prior_distr <- "nonsense"
  expect_error(
    check_clock_model(rln_clock_model),
    "'clock_model\\$mean_rate_prior_distr' must be a distribution"
  )

  rln_clock_model <- create_rln_clock_model()
  rln_clock_model$rate_scaler_factor <- "nonsense"
  expect_error(
    check_clock_model(rln_clock_model),
    "'clock_model\\$rate_scaler_factor' must be a number"
  )

})
