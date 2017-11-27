context("create_site_model")

test_that("JC69 is accepted", {

  testthat::expect_true(is_site_model(create_jc69_site_model()))

})

test_that("abuse", {

  testthat::expect_error(
    beautier::create_site_model(
      name = "nonsense",
      id = "OK"
    ),
    "'site model' must be a site model name, which is one of these: "
  )

  testthat::expect_error(
    beautier::create_site_model(
      name = "JC69",
      id = ape::rcoal(3)
    ),
    "'id' must be a valid ID"
  )

  testthat::expect_error(
    create_site_model(
      name = "JC69",
      id = "OK",
      gamma_site_model = "nonsense"
    ),
    "'gamma_site_model' must be a valid gamma site model"
  )

})

test_that("use more typesafe names", {

  site_model <- beautier::create_jc69_site_model()
  testthat::expect_true(beautier::is_site_model(site_model))

  site_model <- beautier::create_hky_site_model()
  testthat::expect_true(beautier::is_site_model(site_model))

  site_model <- beautier::create_tn93_site_model()
  testthat::expect_true(beautier::is_site_model(site_model))

  site_model <- beautier::create_gtr_site_model()
  testthat::expect_true(beautier::is_site_model(site_model))

})

test_that("Can specify HKY kappa", {

  site_model <- beautier::create_hky_site_model(kappa = 2.0)
  testthat::expect_true(beautier::is_site_model(site_model))
  testthat::expect_equal(get_kappa(site_model), 2.0)

})
