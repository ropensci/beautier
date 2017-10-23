context("create_site_models")

test_that("all site_models must be recognized as such", {

  site_models <- beastscriptr::create_site_models()
  testthat::expect_true(length(site_models) > 1)
  for (site_model in site_models) {
    testthat::expect_true(beastscriptr::is_site_model(site_model))
  }

})
