context("create_site_model")

test_that("JC69 is accepted", {

  site_model <- beastscriptr::create_site_model(name = "JC69")
  testthat::expect_true(beastscriptr::is_site_model(site_model))

})
