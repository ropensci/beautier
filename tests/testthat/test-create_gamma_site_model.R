context("create_gamma_site_model")

test_that("Can specify HKY gamma category count", {

  gamma_site_model <- beautier::create_gamma_site_model(gamma_cat_count = 1)
  testthat::expect_true(is_gamma_site_model(gamma_site_model))
  testthat::expect_equal(gamma_site_model$gamma_cat_count, 1)

})

test_that("Can specify HKY proportion invariant", {

  gamma_site_model <- beautier::create_gamma_site_model(
    prop_invariant = 0.2)
  testthat::expect_true(beautier:::is_gamma_site_model(gamma_site_model))
  testthat::expect_equal(gamma_site_model$prop_invariant, 0.2)

})
