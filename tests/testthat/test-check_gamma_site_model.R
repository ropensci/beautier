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
    "'gamma_cat_count' must be an element of a 'gamma_site_model'"
  )

  expect_error(
    check_gamma_site_model(
      create_gamma_site_model(gamma_cat_count = c(1, 2))
    ),
    "'gamma_cat_count' must be one number"
  )
  expect_error(
    check_gamma_site_model(
      create_gamma_site_model(gamma_shape = c(1, 2))
    ),
    "'gamma_shape' must be one number"
  )
  expect_error(
    check_gamma_site_model(
      create_gamma_site_model(prop_invariant = c(1, 2))
    ),
    "'prop_invariant' must be one number"
  )

})
