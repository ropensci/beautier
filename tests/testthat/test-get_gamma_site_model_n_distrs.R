context("test-get_gamma_site_model_n_distrs")

test_that("use", {

  expect_equal(
    get_gamma_site_model_n_distrs(
      create_gamma_site_model(gamma_cat_count = 0)
    ),
    0
  )

  expect_equal(
    get_gamma_site_model_n_distrs(
      create_gamma_site_model(gamma_cat_count = 1)
    ),
    0
  )

  # 1 distr with 0 params
  expect_equal(
    get_gamma_site_model_n_distrs(
      create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape_prior_distr = create_uniform_distr()
      )
    ),
    1
  )

  # 1 distr with 1 param
  expect_equal(
    get_gamma_site_model_n_distrs(
      create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape_prior_distr = create_poisson_distr()
      )
    ),
    1
  )

  # 1 distr with 2 params
  expect_equal(
    get_gamma_site_model_n_distrs(
      create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape_prior_distr = create_normal_distr()
      )
    ),
    1
  )

})
