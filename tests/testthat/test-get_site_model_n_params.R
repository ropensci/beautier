context("get_site_model_n_params")

test_that("use, GTR", {

  # gamma_0_alpha, gamma_0_beta,
  # gamma_1_alpha, gamma_1_beta,
  # gamma_2_alpha, gamma_2_beta,
  # gamma_3_alpha, gamma_3_beta
  # gamma_5_alpha, gamma_5_beta
  testthat::expect_equal(get_site_model_n_params(create_gtr_site_model()), 10)

})

test_that("use, HKY", {

  # kappa_prior_distr: log_normal_distr: M and S
  testthat::expect_equal(get_site_model_n_params(create_hky_site_model()), 2)

  # kappa_prior_distr: exponential_distr: mean
  testthat::expect_equal(
    get_site_model_n_params(
      create_hky_site_model(kappa_prior_distr = create_exp_distr())
    ),
    1
  )

})

test_that("use, JC69", {

  # no distributions, thus no parameters
  testthat::expect_equal(get_site_model_n_params(create_jc69_site_model()), 0)

})

test_that("use, TN93", {

  # kappa_1_prior_distr: log_normal_distr: M and S
  # kappa_2_prior_distr: log_normal_distr: M and S
  testthat::expect_equal(get_site_model_n_params(create_tn93_site_model()), 4)

  # kappa_1_prior_distr: exp_distr: mean
  # kappa_2_prior_distr: exp_distr: mean
  testthat::expect_equal(
    get_site_model_n_params(
      create_tn93_site_model(
        kappa_1_prior_distr = create_exp_distr(),
        kappa_2_prior_distr = create_exp_distr()
      )
    ),
    2
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
