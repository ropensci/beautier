context("test-check_clock_model")

test_that("use", {
  expect_silent(check_clock_model(create_strict_clock_model()))
  expect_silent(check_clock_model(create_rln_clock_model()))

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
})
