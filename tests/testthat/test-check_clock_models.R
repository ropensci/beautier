test_that("use", {
  expect_silent(
    check_clock_models(
      create_strict_clock_model()
    )
  )
  expect_silent(
    check_clock_models(
      list(create_strict_clock_model())
    )
  )
  expect_silent(
    check_clock_models(
      list(create_strict_clock_model(), create_rln_clock_model())
    )
  )
  expect_error(
    check_clock_models("nonsense"),
    "'clock_models' must be a list of one or more valid clock models"
  )
  expect_error(
    check_clock_models(NULL),
    "'clock_models' must be a list of one or more valid clock models"
  )
  expect_error(
    check_clock_models(NA),
    "'clock_models' must be a list of one or more valid clock models"
  )
  expect_error(
    check_clock_models(
      list(create_strict_clock_model(), "nonsense")
    ),
    "'clock_models' must be a list of one or more valid clock models"
  )
})
