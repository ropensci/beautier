test_that("use", {

  gamma_site_model <- create_gamma_site_model(gamma_cat_count = 2)
  expect_true(get_gamma_site_model_n_distrs(gamma_site_model) == 1)

  site_model_0_distrs <- create_jc69_site_model(
    gamma_site_model = gamma_site_model
  )
  site_model_1_distrs <- create_hky_site_model(
    gamma_site_model = gamma_site_model
  )
  site_model_2_distrs <- create_tn93_site_model(
    gamma_site_model = gamma_site_model
  )

  expect_true(get_site_model_n_distrs(site_model_0_distrs) == 1)
  expect_true(get_site_model_n_distrs(site_model_1_distrs) == 2)
  expect_true(get_site_model_n_distrs(site_model_2_distrs) == 3)

  expect_equal(
    get_site_models_n_distrs(
      list(site_model_0_distrs, site_model_1_distrs)
    ),
    1 + 2
  )
  expect_equal(
    get_site_models_n_distrs(
      list(site_model_1_distrs, site_model_2_distrs)
    ),
    2 + 3
  )

})

test_that("abuse", {

  expect_error(
    get_site_models_n_distrs("nonsense"),
    "'site_models' must be a list of site models"
  )

})
