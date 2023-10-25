context("get_site_models_n_params")

test_that("use", {

  gamma_site_model <- create_gamma_site_model(
    gamma_cat_count = 2, gamma_shape_prior_distr = create_exp_distr()
  )

  site_model_0_params <- create_jc69_site_model(
    gamma_site_model = gamma_site_model
  )
  site_model_2_params <- create_hky_site_model(
    gamma_site_model = gamma_site_model
  )
  site_model_4_params <- create_tn93_site_model(
    gamma_site_model = gamma_site_model
  )
  site_model_10_params <- create_gtr_site_model(
    gamma_site_model = gamma_site_model
  )
  expect_true(get_site_model_n_params(site_model_0_params) == 1)
  expect_true(get_site_model_n_params(site_model_2_params) == 3)
  expect_true(get_site_model_n_params(site_model_4_params) == 5)
  expect_true(get_site_model_n_params(site_model_10_params) == 11)

  testthat::expect_equal(
    get_site_models_n_params(
      list(site_model_0_params, site_model_2_params)
    ),
    1 + 3
  )

  testthat::expect_equal(
    get_site_models_n_params(
      list(site_model_4_params, site_model_10_params)
    ),
    5 + 11
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_site_models_n_params("nonsense"),
    "'site_models' must be a list of site models"
  )

})
