context("is_jc69_site_model")

test_that("use", {

  testthat::expect_true(is_jc69_site_model(create_jc69_site_model()))
  testthat::expect_false(is_jc69_site_model(create_hky_site_model()))

})
