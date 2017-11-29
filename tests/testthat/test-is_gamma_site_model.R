context("is_gamma_site_model")

test_that("use", {

  testthat::expect_true(is_gamma_site_model(create_gamma_site_model()))

  testthat::expect_false(
    is_gamma_site_model(
      create_gamma_site_model(gamma_cat_count = -1)
    )
  )

  testthat::expect_false(
    is_gamma_site_model(
      create_gamma_site_model(gamma_shape = -1)
    )
  )

  testthat::expect_false(
    is_gamma_site_model(
      create_gamma_site_model(prop_invariant = -0.5)
    )
  )
  testthat::expect_false(
    is_gamma_site_model(
      create_gamma_site_model(prop_invariant = 1.5)
    )
  )
  testthat::expect_false(
    is_gamma_site_model(
      create_gamma_site_model(gamma_shape_prior_distr = "nonsense")
    )
  )

  testthat::expect_false(is_gamma_site_model("nonsense"))
  testthat::expect_false(is_gamma_site_model(NA))
  testthat::expect_false(is_gamma_site_model(NULL))

})
