context("create_site_models")

test_that("create default site_models", {

  testthat::expect_silent(
    beastscriptr::create_site_models()
  )

})
