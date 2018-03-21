context("init_gamma_site_model")

test_that("use", {

  gamma_site_model <- create_gamma_site_model(
    gamma_shape_prior_distr = create_one_div_x_distr(id = NA)
  )
  testit::assert(!beautier:::is_init_gamma_site_model(gamma_site_model))
  gamma_site_model <- beautier:::init_gamma_site_model(gamma_site_model)
  testthat::expect_true(beautier:::is_init_gamma_site_model(gamma_site_model))


})
