context("get_site_models_n_params")

test_that("use", {

  site_model_0_params <- create_jc69_site_model()
  site_model_2_params <- create_hky_site_model()
  site_model_4_params <- create_tn93_site_model()
  site_model_10_params <- create_gtr_site_model()
  testit::assert(beautier:::get_site_model_n_params(site_model_0_params) == 1)
  testit::assert(beautier:::get_site_model_n_params(site_model_2_params) == 3)
  testit::assert(beautier:::get_site_model_n_params(site_model_4_params) == 5)
  testit::assert(beautier:::get_site_model_n_params(site_model_10_params) == 11)

  testthat::expect_equal(
    beautier:::get_site_models_n_params(
      list(site_model_0_params, site_model_2_params)
    ),
    1 + 3
  )

  testthat::expect_equal(
    beautier:::get_site_models_n_params(
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
