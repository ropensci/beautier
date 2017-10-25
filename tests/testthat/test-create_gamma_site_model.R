context("create_gamma_site_model")

test_that("Can specify HKY gamma category count", {

  gamma_site_model <- beastscriptr::create_gamma_site_model(gamma_cat_count = 1)
  testthat::expect_true(beastscriptr::is_gamma_site_model(gamma_site_model))
  testthat::expect_equal(get_gamma_cat_count(gamma_site_model), 1)

})

test_that("Can specify HKY proportion invariant", {

  gamma_site_model <- beastscriptr::create_gamma_site_model(
    prop_invariant = 0.2)
  testthat::expect_true(beastscriptr::is_gamma_site_model(gamma_site_model))
  testthat::expect_equal(get_prop_invariant(gamma_site_model), 0.2)

})
