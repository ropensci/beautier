context("create_gamma_site_model")

test_that("Can specify HKY gamma category count", {

  gamma_site_model <- beautier::create_gamma_site_model(gamma_cat_count = 1)
  testthat::expect_true(is_gamma_site_model(gamma_site_model))
  testthat::expect_equal(gamma_site_model$gamma_cat_count, 1)

})

test_that("Can specify HKY proportion invariant", {

  gamma_site_model <- beautier::create_gamma_site_model(
    prop_invariant = 0.2)
  testthat::expect_true(beautier:::is_gamma_site_model(gamma_site_model))
  testthat::expect_equal(gamma_site_model$prop_invariant, 0.2)

})

test_that("zero gamma count categories has no distribution", {

  gamma_site_model <- create_gamma_site_model(gamma_cat_count = 0)
  expect_false(
    is_distr(gamma_site_model$gamma_shape_prior_distr)
  )
})

test_that("one gamma count categories has no distribution", {

  gamma_site_model <- create_gamma_site_model(gamma_cat_count = 1)
  expect_false(
    is_distr(gamma_site_model$gamma_shape_prior_distr)
  )
})

test_that("two gamma count categories has a distribution", {

  gamma_site_model <- create_gamma_site_model(gamma_cat_count = 2)
  expect_true(
    is_distr(gamma_site_model$gamma_shape_prior_distr)
  )
})


test_that("abuse", {

  testthat::expect_silent(
    create_gamma_site_model()
  )

  testthat::expect_error(
    create_gamma_site_model(gamma_cat_count = -1),
    "'gamma_cat_count' must be positive"

  )

  testthat::expect_error(
    create_gamma_site_model(gamma_shape = -1),
    "'gamma_shape' must be positive"

  )

  testthat::expect_error(
    create_gamma_site_model(prop_invariant = -0.5),
    "'prop_invariant' must be in range \\[0.0, 1.0\\]"
  )

  testthat::expect_error(
    create_gamma_site_model(prop_invariant = 1.5),
    "'prop_invariant' must be in range \\[0.0, 1.0\\]"
  )

  testthat::expect_error(
    create_gamma_site_model(gamma_shape_prior_distr = "nonsense"),
    "'gamma_shape_prior_distr' must be a distribution"
  )

  testthat::expect_error(
    create_gamma_site_model(
      gamma_cat_count = 0,
      gamma_shape_prior_distr = create_exp_distr()
    ),
    paste0(
      "'gamma_shape_prior_distr' must be NA ",
      "for a 'gamma_cat_count' of less than two"
    )
  )
  testthat::expect_error(
    create_gamma_site_model(
      gamma_cat_count = 1,
      gamma_shape_prior_distr = create_exp_distr()
    ),
    paste0(
      "'gamma_shape_prior_distr' must be NA ",
      "for a 'gamma_cat_count' of less than two"
    )
  )

})
