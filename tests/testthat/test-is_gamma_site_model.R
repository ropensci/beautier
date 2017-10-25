context("is_gamma_site_model")

test_that("use", {

  testthat::expect_true(is_gamma_site_model(get_default_gamma_site_model()))
  testthat::expect_false(is_gamma_site_model("nonsense"))
  testthat::expect_false(is_gamma_site_model(NA))
  testthat::expect_false(is_gamma_site_model(NULL))

})
