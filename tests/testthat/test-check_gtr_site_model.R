test_that("devious", {

  g <- create_gtr_site_model()
  expect_silent(check_gtr_site_model(g))

  # No site model
  h <- g
  h <- "nonsense"
  expect_error(check_gtr_site_model(h))

  # Invalid name
  h <- g
  h$name <- "nonsense"
  expect_error(check_gtr_site_model(h))

  # No 'rate_ac_prior_distr'
  h <- g[names(g) != "rate_ac_prior_distr"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_ag_prior_distr'
  h <- g[names(g) != "rate_ag_prior_distr"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_at_prior_distr'
  h <- g[names(g) != "rate_at_prior_distr"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_cg_prior_distr'
  h <- g[names(g) != "rate_cg_prior_distr"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_gt_prior_distr'
  h <- g[names(g) != "rate_gt_prior_distr"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_ac_param'
  h <- g[names(g) != "rate_ac_param"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_ag_param'
  h <- g[names(g) != "rate_ag_param"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_at_param'
  h <- g[names(g) != "rate_at_param"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_cg_param'
  h <- g[names(g) != "rate_cg_param"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_ct_param'
  h <- g[names(g) != "rate_ct_param"]
  expect_error(check_gtr_site_model(h))

  # No 'rate_gt_param'
  h <- g[names(g) != "rate_gt_param"]
  expect_error(check_gtr_site_model(h))

  # No 'freq_equilibrium'
  h <- g[names(g) != "freq_equilibrium"]
  expect_error(check_gtr_site_model(h))

  # Invalid distributions
  h <- g
  h$rate_ac_prior_distr <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_ag_prior_distr <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_at_prior_distr <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_cg_prior_distr <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_gt_prior_distr <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_ac_param <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_ag_param <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_at_param <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_cg_param <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_ct_param <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$rate_gt_param <- "nonsense"
  expect_error(check_gtr_site_model(h))

  h <- g
  h$freq_equilibrium <- "nonsense"
  expect_error(check_gtr_site_model(h))
})
