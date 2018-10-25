context("is_init_gamma_site_model")

test_that("use, no distribution", {

  # For a gamma cat count less than two, there is no distribution,
  # thus always initialized
  testthat::expect_true(
    beautier:::is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 0
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 1
      )
    )
  )
})

test_that("use, a distribution", {

  testthat::expect_true(
    beautier:::is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape_prior_distr = create_one_div_x_distr(id = 1)
      )
    )
  )

  testthat::expect_false(
    beautier:::is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape_prior_distr = create_one_div_x_distr(id = NA)
      )
    )
  )

})
