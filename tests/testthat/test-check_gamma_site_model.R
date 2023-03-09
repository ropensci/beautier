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


test_that("devious", {

  g <- create_gamma_site_model()
  expect_silent(check_gamma_site_model(g))

  # No site model
  h <- g
  h <- "nonsense"
  expect_error(check_gamma_site_model(h))

  # No 'gamma_cat_count'
  h <- g[names(g) != "gamma_cat_count"]
  expect_error(check_gamma_site_model(h))

  # No 'gamma_shape'
  h <- g[names(g) != "gamma_shape"]
  expect_error(check_gamma_site_model(h))

  # No 'prop_invariant'
  h <- g[names(g) != "prop_invariant"]
  expect_error(check_gamma_site_model(h))

  # No 'gamma_shape_prior_distr'
  h <- g[names(g) != "gamma_shape_prior_distr"]
  expect_error(check_gamma_site_model(h))

  # No 'freq_equilibrium'
  h <- g[names(g) != "freq_equilibrium"]
  expect_error(check_gamma_site_model(h))

  # No 'freq_prior_uniform_distr_id'
  h <- g[names(g) != "freq_prior_uniform_distr_id"]
  expect_error(check_gamma_site_model(h))

})
