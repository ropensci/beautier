context("is_gamma_site_model")

test_that("use", {

  testthat::expect_true(is_gamma_site_model(get_default_gamma_site_model()))
  testthat::expect_false(is_gamma_site_model("nonsense"))
  testthat::expect_false(is_gamma_site_model(NA))
  testthat::expect_false(is_gamma_site_model(NULL))
  testthat::expect_false(is_gamma_site_model(
    list(gamma_cat_count = 3)
  ))
  testthat::expect_false(is_gamma_site_model(
    list(gamma_cat_count = 3, gamma_shape = 1.5)
  ))

  # OK again, using hand-made list
  testthat::expect_true(is_gamma_site_model(
    list(gamma_cat_count = 3, gamma_shape = 1.5, prop_invariant = 0.5)
  ))

  testthat::expect_false(is_gamma_site_model(
    list(gamma_cat_count = -1, gamma_shape = 1.5, prop_invariant = 0.5)
  ))

  testthat::expect_false(is_gamma_site_model(
    list(gamma_cat_count = 3, gamma_shape = -1.5, prop_invariant = 0.5)
  ))

  testthat::expect_false(is_gamma_site_model(
    list(gamma_cat_count = 3, gamma_shape = 1.5, prop_invariant = -0.5)
  ))

  testthat::expect_false(is_gamma_site_model(
    list(gamma_cat_count = 3, gamma_shape = 1.5, prop_invariant = 1.5)
  ))

})
