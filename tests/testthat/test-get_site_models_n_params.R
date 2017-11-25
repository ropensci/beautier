context("get_site_models_n_params")

test_that("use", {

  site_model_0_params <- create_jc69_site_model()
  site_model_2_params <- create_hky_site_model()
  site_model_4_params <- create_tn93_site_model()
  site_model_10_params <- create_gtr_site_model()
  testit::assert(get_site_model_n_params(site_model_0_params) == 0)
  testit::assert(get_site_model_n_params(site_model_2_params) == 2)
  testit::assert(get_site_model_n_params(site_model_4_params) == 4)
  testit::assert(get_site_model_n_params(site_model_10_params) == 10)

  testthat::expect_equal(
    get_site_models_n_params(
      list(site_model_0_params, site_model_2_params)
    ),
    2
  )

  testthat::expect_equal(
    get_site_models_n_params(
      list(site_model_4_params, site_model_10_params)
    ),
    14
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_site_models_n_params("nonsense"),
    "'site_models' must be a list of site models"
  )

})
