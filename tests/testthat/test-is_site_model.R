context("is_site_model")

test_that("use", {

  expect_true(is_site_model(create_jc69_site_model()))

  expect_false(is_site_model(NA))
  expect_false(is_site_model(NULL))
  expect_false(is_site_model("nonsense"))
  expect_false(is_site_model(list(name = "nonsense")))
  expect_false(is_site_model(list(name = "JC69")))

  expect_false(is_gtr_site_model(NA))
  expect_false(is_gtr_site_model(NULL))
  expect_false(is_gtr_site_model("nonsense"))

  expect_false(is_hky_site_model(NA))
  expect_false(is_hky_site_model(NULL))
  expect_false(is_hky_site_model("nonsense"))

  expect_false(is_jc69_site_model(NA))
  expect_false(is_jc69_site_model(NULL))
  expect_false(is_jc69_site_model("nonsense"))

  expect_false(is_tn93_site_model(NA))
  expect_false(is_tn93_site_model(NULL))
  expect_false(is_tn93_site_model("nonsense"))
})

test_that("is_site_model: devious", {

  g <- create_jc69_site_model()
  expect_true(is_site_model(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_site_model(h))

  # Invald 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_site_model(h))

  # No 'id'
  h <- g[names(g) != "id"]
  expect_false(is_site_model(h))

  # No 'gamma_site_model'
  h <- g[names(g) != "gamma_site_model"]
  expect_false(is_site_model(h))

  # Invalid 'gamma_site_model'
  h <- g
  h$gamma_site_model <- "nonsense"
  expect_false(is_site_model(h))
})

test_that("is_gtr_site_model: devious", {

  g <- create_gtr_site_model()
  expect_true(is_gtr_site_model(g))

  # No site model
  h <- g
  h <- "nonsense"
  expect_false(is_gtr_site_model(h))

  # Invalid name
  h <- g
  h$name <- "nonsense"
  expect_false(is_gtr_site_model(h))

  # No 'rate_ac_prior_distr'
  h <- g[names(g) != "rate_ac_prior_distr"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_ag_prior_distr'
  h <- g[names(g) != "rate_ag_prior_distr"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_at_prior_distr'
  h <- g[names(g) != "rate_at_prior_distr"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_cg_prior_distr'
  h <- g[names(g) != "rate_cg_prior_distr"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_gt_prior_distr'
  h <- g[names(g) != "rate_gt_prior_distr"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_ac_param'
  h <- g[names(g) != "rate_ac_param"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_ag_param'
  h <- g[names(g) != "rate_ag_param"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_at_param'
  h <- g[names(g) != "rate_at_param"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_cg_param'
  h <- g[names(g) != "rate_cg_param"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_ct_param'
  h <- g[names(g) != "rate_ct_param"]
  expect_false(is_gtr_site_model(h))

  # No 'rate_gt_param'
  h <- g[names(g) != "rate_gt_param"]
  expect_false(is_gtr_site_model(h))

  # No 'freq_equilibrium'
  h <- g[names(g) != "freq_equilibrium"]
  expect_false(is_gtr_site_model(h))

  # Invalid distributions
  h <- g
  h$rate_ac_prior_distr <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_ag_prior_distr <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_at_prior_distr <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_cg_prior_distr <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_gt_prior_distr <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_ac_param <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_ag_param <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_at_param <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_cg_param <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_ct_param <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$rate_gt_param <- "nonsense"
  expect_false(is_gtr_site_model(h))

  h <- g
  h$freq_equilibrium <- "nonsense"
  expect_false(is_gtr_site_model(h))
})

test_that("is_hky_site_model: devious", {

  g <- create_hky_site_model()
  expect_true(is_hky_site_model(g))

  # No site model
  h <- g
  h <- "nonsense"
  expect_false(is_hky_site_model(h))

  # Invalid name
  h <- g
  h$name <- "nonsense"
  expect_false(is_hky_site_model(h))

  # No 'kappa'
  h <- g[names(g) != "kappa"]
  expect_false(is_hky_site_model(h))

  # No 'kappa_prior_distr'
  h <- g[names(g) != "kappa_prior_distr"]
  expect_false(is_hky_site_model(h))

  # Invalid 'kappa_prior_distr'
  h <- g
  h$kappa_prior_distr <- "nonsense"
  expect_false(is_hky_site_model(h))

  # No 'freq_equilibrium'
  h <- g[names(g) != "freq_equilibrium"]
  expect_false(is_hky_site_model(h))

  # Invalid 'freq_equilibrium'
  h <- g
  h$freq_equilibrium <- "nonsense"
  expect_false(is_hky_site_model(h))
})

test_that("is_tn93_site_model: devious", {

  g <- create_tn93_site_model()
  expect_true(is_tn93_site_model(g))

  # No site model
  h <- g
  h <- "nonsense"
  expect_false(is_tn93_site_model(h))

  # Invalid name
  h <- g
  h$name <- "nonsense"
  expect_false(is_tn93_site_model(h))

  # No 'kappa_1_prior_distr'
  h <- g[names(g) != "kappa_1_prior_distr"]
  expect_false(is_tn93_site_model(h))

  # Invalid 'kappa_1_prior_distr'
  h <- g
  h$kappa_1_prior_distr <- "nonsense"
  expect_false(is_tn93_site_model(h))

  # No 'kappa_2_prior_distr'
  h <- g[names(g) != "kappa_2_prior_distr"]
  expect_false(is_tn93_site_model(h))

  # Invalid 'kappa_2_prior_distr'
  h <- g
  h$kappa_2_prior_distr <- "nonsense"
  expect_false(is_tn93_site_model(h))

  # No 'kappa_1_param'
  h <- g[names(g) != "kappa_1_param"]
  expect_false(is_tn93_site_model(h))

  # Invalid 'kappa_1_param'
  h <- g
  h$kappa_1_param <- "nonsense"
  expect_false(is_tn93_site_model(h))

  # No 'kappa_2_param'
  h <- g[names(g) != "kappa_2_param"]
  expect_false(is_tn93_site_model(h))

  # Ivalid 'kappa_2_param'
  h <- g
  h$kappa_2_param <- "nonsense"
  expect_false(is_tn93_site_model(h))

  # No 'freq_equilibrium'
  h <- g[names(g) != "freq_equilibrium"]
  expect_false(is_tn93_site_model(h))

  # Invalid 'freq_equilibrium'
  h <- g
  h$freq_equilibrium <- "nonsense"
  expect_false(is_tn93_site_model(h))
})
