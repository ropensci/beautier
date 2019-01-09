context("test-check_site_models")

test_that("use", {
  expect_silent(
    check_site_models(
      create_jc69_site_model()
    )
  )
  expect_silent(
    check_site_models(
      list(create_jc69_site_model())
    )
  )
  expect_silent(
    check_site_models(
      list(create_jc69_site_model(), create_gtr_site_model())
    )
  )
  expect_error(
    check_site_models("nonsense"),
    "'site_models' must be a list of one or more valid site models"
  )
  expect_error(
    check_site_models(NULL),
    "'site_models' must be a list of one or more valid site models"
  )
  expect_error(
    check_site_models(NA),
    "'site_models' must be a list of one or more valid site models"
  )
})
