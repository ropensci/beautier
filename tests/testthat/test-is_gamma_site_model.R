context("is_gamma_site_model")

test_that("use", {

  testthat::expect_true(beautier:::is_gamma_site_model(
    create_gamma_site_model())
  )
  testthat::expect_false(beautier:::is_gamma_site_model("nonsense"))
  testthat::expect_false(beautier:::is_gamma_site_model(NA))
  testthat::expect_false(beautier:::is_gamma_site_model(NULL))

})
