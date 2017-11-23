context("get_site_model_n_distrs")

test_that("use", {

  # no distributions
  testthat::expect_equal(get_site_model_n_distrs(create_gtr_site_model()), 0)

  # kappa_prior_distr
  testthat::expect_equal(get_site_model_n_distrs(create_hky_site_model()), 1)

  # no distributions
  testthat::expect_equal(get_site_model_n_distrs(create_jc69_site_model()), 0)

  # kappa_1_prior_distr and kappa_2_prior_distr
  testthat::expect_equal(get_site_model_n_distrs(create_tn93_site_model()), 2)

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
