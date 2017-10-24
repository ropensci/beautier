context("create_site_model")

test_that("JC69 is accepted", {

  site_model <- beastscriptr::create_site_model(name = "JC69")
  testthat::expect_true(beastscriptr::is_site_model(site_model))

})

test_that("abuse", {

  testthat::expect_error(
    beastscriptr::create_site_model(name = "nonsense")
  )

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

  site_model <- beastscriptr::create_hky_site_model(kappa = 2.0)
  testthat::expect_true(beastscriptr::is_site_model(site_model))
  testthat::expect_equal(get_kappa(site_model), 2.0)

})

test_that("Can specify HKY gamma category count", {

  site_model <- beastscriptr::create_hky_site_model(gamma_cat_count = 1)
  testthat::expect_true(beastscriptr::is_site_model(site_model))
  testthat::expect_equal(get_gamma_cat_count(site_model), 1)

})

test_that("Can specify HKY proportion invariant", {

  site_model <- beastscriptr::create_hky_site_model(prop_invariant = 0.2)
  testthat::expect_true(beastscriptr::is_site_model(site_model))
  testthat::expect_equal(get_prop_invariant(site_model), 0.2)

})
