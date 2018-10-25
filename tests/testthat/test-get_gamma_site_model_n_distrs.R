context("test-get_gamma_site_model_n_distrs")

test_that("use", {

  # All gamme site models have one distribution?

  expect_equal(
    get_gamma_site_model_n_distrs(
      create_gamma_site_model(gamma_shape_prior_distr = create_normal_distr())
    ),
    1
  )

  expect_equal(
    get_gamma_site_model_n_distrs(
      create_gamma_site_model(gamma_shape_prior_distr = create_uniform_distr())
    ),
    1
  )

})
