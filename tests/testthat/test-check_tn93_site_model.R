test_that("use", {

  g <- create_tn93_site_model()
  expect_silent(check_tn93_site_model(g))

  # No site model
  h <- g
  h <- "nonsense"
  expect_error(check_tn93_site_model(h))

  # Invalid name
  h <- g
  h$name <- "nonsense"
  expect_error(check_tn93_site_model(h))

  # No 'kappa_1_prior_distr'
  h <- g[names(g) != "kappa_1_prior_distr"]
  expect_error(check_tn93_site_model(h))

  # Invalid 'kappa_1_prior_distr'
  h <- g
  h$kappa_1_prior_distr <- "nonsense"
  expect_error(check_tn93_site_model(h))

  # No 'kappa_2_prior_distr'
  h <- g[names(g) != "kappa_2_prior_distr"]
  expect_error(check_tn93_site_model(h))

  # Invalid 'kappa_2_prior_distr'
  h <- g
  h$kappa_2_prior_distr <- "nonsense"
  expect_error(check_tn93_site_model(h))

  # No 'kappa_1_param'
  h <- g[names(g) != "kappa_1_param"]
  expect_error(check_tn93_site_model(h))

  # Invalid 'kappa_1_param'
  h <- g
  h$kappa_1_param <- "nonsense"
  expect_error(check_tn93_site_model(h))

  # No 'kappa_2_param'
  h <- g[names(g) != "kappa_2_param"]
  expect_error(check_tn93_site_model(h))

  # Ivalid 'kappa_2_param'
  h <- g
  h$kappa_2_param <- "nonsense"
  expect_error(check_tn93_site_model(h))

  # No 'freq_equilibrium'
  h <- g[names(g) != "freq_equilibrium"]
  expect_error(check_tn93_site_model(h))

  # Invalid 'freq_equilibrium'
  h <- g
  h$freq_equilibrium <- "nonsense"
  expect_error(check_tn93_site_model(h))
})
