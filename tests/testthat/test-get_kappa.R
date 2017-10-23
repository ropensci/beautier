context("get_kappa")

test_that("use", {

  testthat::expect_equal(get_kappa(create_hky_site_model()), 2.0)

  testthat::expect_equal(get_kappa(create_hky_site_model(kappa = 1.2)), 1.2)
})

test_that("abuse", {

  testthat::expect_error(get_kappa("nonsense"))
})
