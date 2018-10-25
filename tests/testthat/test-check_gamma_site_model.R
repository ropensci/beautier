context("test-check_gamma_site_model")

test_that("use", {
  expect_silent(
    check_gamma_site_model(
      create_gamma_site_model()
    )
  )
  expect_error(
    check_gamma_site_model(
      "nonsense"
    ),
    "'gamma_site_model' must be a valid gamma site model"
  )
})
