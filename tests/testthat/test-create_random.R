context("create_random")


test_that("create_rnd_alpha_param", {
  testthat::expect_true(
    beautier:::is_alpha_param(
      beautier:::create_rnd_alpha_param()
    )
  )
})

test_that("create_rnd_bd_tree_prior", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_bd_tree_prior(
      beautier:::create_rnd_bd_tree_prior()
    )
  )
})

test_that("create_rnd_beta_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_beta_distr(
      beautier:::create_rnd_beta_distr()
    )
  )
})

test_that("create_rnd_beta_param", {
  testthat::expect_true(
    beautier:::is_beta_param(
      beautier:::create_rnd_beta_param()
    )
  )
})

test_that("create_rnd_bool", {
  testthat::expect_true(beautier:::create_rnd_bool() %in% c(TRUE, FALSE))
})

test_that("create_rnd_cbs_tree_prior", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_cbs_tree_prior(
      beautier:::create_rnd_cbs_tree_prior()
    )
  )
})

test_that("create_rnd_ccp_tree_prior", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_ccp_tree_prior(
      beautier:::create_rnd_ccp_tree_prior()
    )
  )
})

test_that("create_rnd_cep_tree_prior", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_cep_tree_prior(
      beautier:::create_rnd_cep_tree_prior()
    )
  )
})

test_that("create_rnd_clock_model", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 5)) {
    testthat::expect_true(
      beautier:::is_clock_model(
        beautier:::create_rnd_clock_model()
      )
    )
  }
})

test_that("create_rnd_clock_rate_param", {
  testthat::expect_true(
    beautier:::is_clock_rate_param(
      beautier:::create_rnd_clock_rate_param()
    )
  )
})

test_that("create_rnd_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_distr(
      beautier:::create_rnd_distr()
    )
  )
})

test_that("create_rnd_estimate", {
  testthat::expect_true(beautier:::create_rnd_estimate() %in% c(TRUE, FALSE))
})

test_that("create_rnd_exp_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_exp_distr(
      beautier:::create_rnd_exp_distr()
    )
  )
})

test_that("create_rnd_freq_equilibrium", {
  testthat::expect_true(beautier:::create_rnd_freq_equilibrium()
    %in% c("estimated", "empirical", "all_equal")
  )
})

test_that("create_rnd_gamma_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_gamma_distr(
      beautier:::create_rnd_gamma_distr()
    )
  )
})

test_that("create_rnd_gamma_site_model", {
  set.seed(0)
  for (i in seq(1, 3)) {
    testthat::expect_true(
      beautier:::is_gamma_site_model(
        beautier:::create_rnd_gamma_site_model()
      )
    )
  }
})

test_that("create_rnd_gtr_site_model", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_gtr_site_model(
      beautier:::create_rnd_gtr_site_model()
    )
  )
})

test_that("create_rnd_hky_site_model", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_hky_site_model(
      beautier:::create_rnd_hky_site_model()
    )
  )
})

test_that("create_rnd_inv_gamma_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_inv_gamma_distr(
      beautier:::create_rnd_inv_gamma_distr()
    )
  )
})

test_that("create_rnd_jc69_site_model", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_jc69_site_model(
      beautier:::create_rnd_jc69_site_model()
    )
  )
})

test_that("create_rnd_kappa_1_param", {
  testthat::expect_true(
    beautier:::is_kappa_1_param(
      beautier:::create_rnd_kappa_1_param()
    )
  )
})

test_that("create_rnd_kappa_2_param", {
  testthat::expect_true(
    beautier:::is_kappa_2_param(
      beautier:::create_rnd_kappa_2_param()
    )
  )
})

test_that("create_rnd_lambda_param", {
  testthat::expect_true(
    beautier:::is_lambda_param(
      beautier:::create_rnd_lambda_param()
    )
  )
})

test_that("create_rnd_laplace_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_laplace_distr(
      beautier:::create_rnd_laplace_distr()
    )
  )
})

test_that("create_rnd_log_normal_distr", {
  set.seed(0)
  for (i in seq(1, 6)) {
    testthat::expect_true(
      beautier:::is_log_normal_distr(
        beautier:::create_rnd_log_normal_distr()
      )
    )
  }
})

test_that("create_rnd_m_param", {
  testthat::expect_true(
    beautier:::is_m_param(
      beautier:::create_rnd_m_param()
    )
  )
})

test_that("create_rnd_mean_param", {
  testthat::expect_true(
    beautier:::is_mean_param(
      beautier:::create_rnd_mean_param()
    )
  )
})

test_that("create_rnd_mrca_prior", {
  testthat::expect_true(
    beautier:::is_mrca_prior(
      beautier:::create_rnd_mrca_prior(
        get_beautier_path("anthus_aco_sub.fas")
      )
    )
  )
})

test_that("create_rnd_mrca_priors", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 6)) {
    testthat::expect_true(
      beautier:::are_mrca_priors(
        beautier:::create_rnd_mrca_priors(
          get_beautier_path("anthus_aco_sub.fas")
        )
      )
    )
  }
})


test_that("create_rnd_mu_param", {
  testthat::expect_true(
    beautier:::is_mu_param(
      beautier:::create_rnd_mu_param()
    )
  )
})

test_that("create_rnd_normal_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_normal_distr(
      beautier:::create_rnd_normal_distr()
    )
  )
})

test_that("create_rnd_one_div_x_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_one_div_x_distr(
      beautier:::create_rnd_one_div_x_distr()
    )
  )
})

test_that("create_rnd_param", {

  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 65)) {
    testthat::expect_true(
      beautier:::is_param(
        beautier:::create_rnd_param()
      )
    )
  }
})

test_that("create_rnd_poisson_distr", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_poisson_distr(
      beautier:::create_rnd_poisson_distr()
    )
  )
})

test_that("create_rnd_rate_ac_param", {
  testthat::expect_true(
    beautier:::is_rate_ac_param(
      beautier:::create_rnd_rate_ac_param()
    )
  )
})

test_that("create_rnd_rate_ag_param", {
  testthat::expect_true(
    beautier:::is_rate_ag_param(
      beautier:::create_rnd_rate_ag_param()
    )
  )
})

test_that("create_rnd_rate_at_param", {
  testthat::expect_true(
    beautier:::is_rate_at_param(
      beautier:::create_rnd_rate_at_param()
    )
  )
})

test_that("create_rnd_rate_cg_param", {
  testthat::expect_true(
    beautier:::is_rate_cg_param(
      beautier:::create_rnd_rate_cg_param()
    )
  )
})

test_that("create_rnd_rate_ct_param", {
  testthat::expect_true(
    beautier:::is_rate_ct_param(
      beautier:::create_rnd_rate_ct_param()
    )
  )
})

test_that("create_rnd_rate_gt_param", {
  testthat::expect_true(
    beautier:::is_rate_gt_param(
      beautier:::create_rnd_rate_gt_param()
    )
  )
})

test_that("create_rnd_rln_clock_model", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_rln_clock_model(
      beautier:::create_rnd_rln_clock_model()
    )
  )
})

test_that("create_rnd_s_param", {
  testthat::expect_true(
    beautier:::is_s_param(
      beautier:::create_rnd_s_param()
    )
  )
})

test_that("create_rnd_scale_param", {
  testthat::expect_true(
    beautier:::is_scale_param(
      beautier:::create_rnd_scale_param()
    )
  )
})

test_that("create_rnd_sigma_param", {
  set.seed(0)
  for (i in seq(1, 2)) {
    testthat::expect_true(
      beautier:::is_sigma_param(
        beautier:::create_rnd_sigma_param()
      )
    )
  }
})

test_that("create_rnd_site_model", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 8)) {
    testthat::expect_true(
      beautier:::is_site_model(
        beautier:::create_rnd_site_model()
      )
    )
  }
})

test_that("create_rnd_strict_clock_model", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_strict_clock_model(
      beautier:::create_rnd_strict_clock_model()
    )
  )
})

test_that("create_rnd_tn93_site_model", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_tn93_site_model(
      beautier:::create_rnd_tn93_site_model()
    )
  )
})

test_that("create_rnd_tree_prior", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 11)) {
    testthat::expect_true(
      beautier:::is_tree_prior(
        beautier:::create_rnd_tree_prior()
      )
    )
  }
})

test_that("create_rnd_two_mrca_priors", {
  set.seed(0)
  # Repeat often enough so all execution branches are hit
  for (i in seq(1, 10)) {
    mrca_priors <- beautier:::create_rnd_two_mrca_priors(
      get_beautier_path("anthus_aco_sub.fas")
    )
    testit::assert(length(mrca_priors) == 2)
    testthat::expect_true(
      beautier:::are_mrca_priors(mrca_priors)
    )
    testthat::expect_true(
      beautier:::are_mrca_taxa_non_intersecting(mrca_priors)
    )

  }
})

test_that("create_rnd_uniform_distr", {
  set.seed(0)
  for (i in seq(1, 2)) {
    testthat::expect_true(
      beautier:::is_uniform_distr(
        beautier:::create_rnd_uniform_distr()
      )
    )
  }
})

test_that("create_rnd_yule_tree_prior", {
  set.seed(0)
  testthat::expect_true(
    beautier:::is_yule_tree_prior(
      beautier:::create_rnd_yule_tree_prior()
    )
  )
})
