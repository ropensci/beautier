context("get_site_model_n_params")

test_that("use, GTR", {

  # gamma_site_model$gamma_shape_prior_distr: exp_distr: 1
  # rate_ac_prior_distr: gamma distribution: 2
  # rate_ag_prior_distr: gamma distribution: 2
  # rate_at_prior_distr: gamma distribution: 2
  # rate_cg_prior_distr: gamma distribution: 2
  # rate_gt_prior_distr: gamma distribution: 2
  testthat::expect_equal(
    beautier:::get_site_model_n_params(create_gtr_site_model()),
    11
  )

  # gamma_site_model$gamma_shape_prior_distr: exp_distr: 1
  # rate_ac_prior_distr: poisson distribution: 1
  # rate_ag_prior_distr: poisson distribution: 1
  # rate_at_prior_distr: poisson distribution: 1
  # rate_cg_prior_distr: poisson distribution: 1
  # rate_gt_prior_distr: poisson distribution: 1
  testthat::expect_equal(
    beautier:::get_site_model_n_params(
      create_gtr_site_model(
        rate_ac_prior_distr = create_poisson_distr(),
        rate_ag_prior_distr = create_poisson_distr(),
        rate_at_prior_distr = create_poisson_distr(),
        rate_cg_prior_distr = create_poisson_distr(),
        rate_gt_prior_distr = create_poisson_distr()
      )
    ),
    6
  )

})

test_that("use, HKY", {

  # gamma_site_model$gamma_shape_prior_distr: exp_distr: 1
  # kappa_prior_distr: log_normal_distr: M and S
  testthat::expect_equal(
    beautier:::get_site_model_n_params(create_hky_site_model()),
    3
  )

  # gamma_site_model$gamma_shape_prior_distr: exp_distr: 1
  # kappa_prior_distr: exponential_distr: mean
  testthat::expect_equal(
    beautier:::get_site_model_n_params(
      create_hky_site_model(kappa_prior_distr = create_exp_distr())
    ),
    2
  )

})

test_that("use, JC69", {

  # gamma_site_model$gamma_shape_prior_distr: exp_distr: 1
  # no distributions, thus no parameters
  testthat::expect_equal(
    beautier:::get_site_model_n_params(create_jc69_site_model()),
    1
  )

})

test_that("use, JC69, different gamma site model", {

  # gamma_site_model$gamma_shape_prior_distr: one_div_x: 0
  testthat::expect_equal(
    beautier:::get_site_model_n_params(
      create_jc69_site_model(
        gamma_site_model = create_gamma_site_model(
          gamma_shape_prior_distr = create_one_div_x_distr()
        )
      )
    ),
    0
  )

})

test_that("use, TN93", {

  # gamma_site_model$gamma_shape_prior_distr: exp_distr: 1
  # kappa_1_prior_distr: log_normal_distr: M and S
  # kappa_2_prior_distr: log_normal_distr: M and S
  testthat::expect_equal(
    beautier:::get_site_model_n_params(create_tn93_site_model()),
    5
  )

  # gamma_site_model$gamma_shape_prior_distr: exp_distr: 1
  # kappa_1_prior_distr: exp_distr: mean
  # kappa_2_prior_distr: exp_distr: mean
  testthat::expect_equal(
    beautier:::get_site_model_n_params(
      create_tn93_site_model(
        kappa_1_prior_distr = create_exp_distr(),
        kappa_2_prior_distr = create_exp_distr()
      )
    ),
    3
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_site_model_n_params("nonsense"),
    "'site_model' must be a site model"
  )
  testthat::expect_error(
    get_site_model_n_params(NA),
    "'site_model' must be a site model"
  )
  testthat::expect_error(
    get_site_model_n_params(NULL),
    "'site_model' must be a site model"
  )
  testthat::expect_error(
    get_site_model_n_params(c()),
    "'site_model' must be a site model"
  )

})
