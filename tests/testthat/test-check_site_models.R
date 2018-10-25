context("test-check_site_models")

test_that("use", {
  expect_silent(
    check_site_models(
      site_models = create_jc69_site_model()
    )
  )
  expect_silent(
    check_site_models(
      site_models = list(create_jc69_site_model())
    )
  )
  expect_silent(
    check_site_models(
      site_models = list(create_jc69_site_model(), create_gtr_site_model())
    )
  )
  expect_error(
    check_site_models(
      site_models = "nonsense"
    ),
    paste0(
      "'site_models' must be a valid site model, ",
      "or a list of valid site models, ",
      "as returned by 'create_site_model'"
    )
  )
})
