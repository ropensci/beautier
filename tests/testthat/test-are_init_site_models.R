context("test-are_init_site_models")

test_that("use", {

  g <- create_jc69_site_model(
    id = 42,
    gamma_site_model = create_gamma_site_model(
      gamma_cat_count = 2,
      gamma_shape_prior_distr = create_one_div_x_distr(id = 0)
    )
  )
  expect_true(is_init_site_model(g))

  expect_true(are_init_site_models(list(g)))

  expect_false(are_init_site_models("nonsense"))

})
