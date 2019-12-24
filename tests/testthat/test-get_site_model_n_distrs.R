context("get_site_model_n_distrs")

test_that("use, default gamma site model", {

  # rate_ac_prior_distr
  # rate_ag_prior_distr
  # rate_at_prior_distr
  # rate_cg_prior_distr
  # rate_gt_prior_distr
  testthat::expect_equal(
    get_site_model_n_distrs(
      create_gtr_site_model()),
    5
  )

  # kappa_prior_distr
  testthat::expect_equal(
    get_site_model_n_distrs(create_hky_site_model()),
    1
  )

  # no other distributions
  testthat::expect_equal(
    get_site_model_n_distrs(create_jc69_site_model()),
    0
  )

  # kappa_1_prior_distr
  # kappa_2_prior_distr
  testthat::expect_equal(
    get_site_model_n_distrs(create_tn93_site_model()),
    2
  )
})

test_that("use, gamma site model with 1 distr", {

  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = 2,
    gamma_shape_prior_distr = create_normal_distr()
  )

  # rate_ac_prior_distr
  # rate_ag_prior_distr
  # rate_at_prior_distr
  # rate_cg_prior_distr
  # rate_gt_prior_distr
  # gamma_site_model$gamma_shape_prior_distr
  testthat::expect_equal(
    get_site_model_n_distrs(
      create_gtr_site_model(
        gamma_site_model = gamma_site_model
      )),
    6
  )

  # kappa_prior_distr
  # gamma_site_model$gamma_shape_prior_distr
  testthat::expect_equal(
    get_site_model_n_distrs(create_hky_site_model(
      gamma_site_model = gamma_site_model
    )),
    2
  )

  # only:
  # gamma_site_model$gamma_shape_prior_distr
  testthat::expect_equal(
    get_site_model_n_distrs(create_jc69_site_model(
      gamma_site_model = gamma_site_model
    )),
    1
  )

  # kappa_1_prior_distr
  # kappa_2_prior_distr
  # gamma_site_model$gamma_shape_prior_distr
  testthat::expect_equal(
    get_site_model_n_distrs(create_tn93_site_model(
      gamma_site_model = gamma_site_model
    )),
    3
  )
})

test_that("abuse", {

  testthat::expect_error(
    get_site_model_n_distrs("nonsense"),
    "'site_model' must be a site model"
  )
  testthat::expect_error(
    get_site_model_n_distrs(NA),
    "'site_model' must be a site model"
  )
  testthat::expect_error(
    get_site_model_n_distrs(NULL),
    "'site_model' must be a site model"
  )
  testthat::expect_error(
    get_site_model_n_distrs(c()),
    "'site_model' must be a site model"
  )

})
