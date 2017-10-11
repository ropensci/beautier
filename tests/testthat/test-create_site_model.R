context("create_site_model")

test_that("create default site_model", {

  testthat::expect_silent(
    beastscriptr::create_site_model()
  )

})
