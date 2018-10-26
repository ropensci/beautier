context("is_init_gamma_site_model")

test_that("use, no distribution", {

  expect_false(
    is_init_gamma_site_model(
      "nonsense"
    )
  )

  # For a gamma cat count less than two, there is no distribution,
  # thus always initialized
  expect_true(
    is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 0
      )
    )
  )
  expect_true(
    is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 1
      )
    )
  )
})

test_that("use, a distribution", {

  expect_true(
    is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape_prior_distr = create_one_div_x_distr(id = 1)
      )
    )
  )

  expect_false(
    is_init_gamma_site_model(
      create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape_prior_distr = create_one_div_x_distr(id = NA)
      )
    )
  )

})
