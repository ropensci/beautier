context("test-check_clock_model")

test_that("use", {
  expect_silent(check_clock_model(create_strict_clock_model()))
  expect_silent(check_clock_model(create_rln_clock_model()))

  # Can use lists
  expect_silent(check_clock_model(list(create_strict_clock_model())))

  # Must be one clock model
  expect_error(
    check_clock_model(
      list(
        create_strict_clock_model(),
        create_strict_clock_model()
      )
    ),
    "'clock_model' must be a valid clock model"
  )

  # Must be a clock model
  expect_error(
    check_clock_model("nonsense"),
    "'clock_model' must be a valid clock model"
  )
  expect_error(
    check_clock_model(NULL),
    "'clock_model' must be a valid clock model"
  )
  expect_error(
    check_clock_model(NA),
    "'clock_model' must be a valid clock model"
  )
})
