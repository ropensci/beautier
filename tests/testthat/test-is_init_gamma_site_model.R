context("is_init_gamma_site_model")

test_that("use", {

  testthat::expect_true(
    beautier:::is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_shape_prior_distr = create_one_div_x_distr(id = 1)
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_shape_prior_distr = create_one_div_x_distr(id = NA)
      )
    )
  )

})
