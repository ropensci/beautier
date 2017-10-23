context("create_site_model")

test_that("JC69 is accepted", {

  site_model <- beastscriptr::create_site_model(name = "JC69")
  testthat::expect_true(beastscriptr::is_site_model(site_model))

})

test_that("abuse", {

  testthat::expect_false(beastscriptr::is_site_model(NA))
  testthat::expect_false(beastscriptr::is_site_model(NULL))
  testthat::expect_false(beastscriptr::is_site_model("nonsense"))
  testthat::expect_false(beastscriptr::is_site_model(list(name = "nonsense")))

})

test_that("use more typesafe names", {

  site_model <- beastscriptr::create_jc69_site_model()
  testthat::expect_true(beastscriptr::is_site_model(site_model))

  site_model <- beastscriptr::create_hky_site_model()
  testthat::expect_true(beastscriptr::is_site_model(site_model))

  site_model <- beastscriptr::create_tn93_site_model()
  testthat::expect_true(beastscriptr::is_site_model(site_model))

  site_model <- beastscriptr::create_gtr_site_model()
  testthat::expect_true(beastscriptr::is_site_model(site_model))

})

test_that("Can specify HKY kappa", {

  skip("WIP")
  site_model <- beastscriptr::create_hky_site_model(kappa = 2.0)
  testthat::expect_true(beastscriptr::is_site_model(site_model))

})
