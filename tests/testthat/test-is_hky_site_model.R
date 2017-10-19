context("is_hky_site_model")

test_that("use", {

  testthat::expect_true(is_hky_site_model(create_site_model(name = "HKY")))
  testthat::expect_false(is_hky_site_model(create_site_model(name = "JC69")))

})
