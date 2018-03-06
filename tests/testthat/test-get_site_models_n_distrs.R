context("get_site_models_n_distrs")

test_that("use", {

  site_model_0_distrs <- create_jc69_site_model()
  site_model_1_distrs <- create_hky_site_model()
  site_model_2_distrs <- create_tn93_site_model()

  testit::assert(beautier:::get_site_model_n_distrs(site_model_0_distrs) == 1)
  testit::assert(beautier:::get_site_model_n_distrs(site_model_1_distrs) == 2)
  testit::assert(beautier:::get_site_model_n_distrs(site_model_2_distrs) == 3)

  testthat::expect_equal(
    beautier:::get_site_models_n_distrs(
      list(site_model_0_distrs, site_model_1_distrs)
    ),
    1 + 2
  )
  testthat::expect_equal(
    beautier:::get_site_models_n_distrs(
      list(site_model_1_distrs, site_model_2_distrs)
    ),
    2 + 3
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_site_models_n_distrs("nonsense"),
    "'site_models' must be a list of site models"
  )

})
