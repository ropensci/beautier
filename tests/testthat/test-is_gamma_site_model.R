context("is_gamma_site_model")

test_that("use", {

  expect_true(is_gamma_site_model(
    create_gamma_site_model())
  )

  expect_false(is_gamma_site_model("nonsense"))
  expect_false(is_gamma_site_model(NA))
  expect_false(is_gamma_site_model(NULL))
})

test_that("devious", {

  g <- create_gamma_site_model()
  expect_true(is_gamma_site_model(g))

  # No gamma_cat_count
  h <- g[names(g) != "gamma_cat_count"]
  expect_false(is_gamma_site_model(h))

  # Invalid gamma_cat_count
  h <- create_gamma_site_model()
  h$gamma_cat_count <- -123
  expect_false(is_gamma_site_model(h))

  # No gamma_shape
  h <- g[names(g) != "gamma_shape"]
  expect_false(is_gamma_site_model(h))

  # Invalid gamma shaoe
  h <- create_gamma_site_model()
  h$gamma_shape <- -123
  expect_false(is_gamma_site_model(h))

  # No prop_invariant
  h <- g[names(g) != "prop_invariant"]
  expect_false(is_gamma_site_model(h))

  # prop_invariant below zero
  h <- create_gamma_site_model()
  h$prop_invariant <- -123.456
  expect_false(is_gamma_site_model(h))

  # prop_invariant above one
  h <- create_gamma_site_model()
  h$prop_invariant <- 123.456
  expect_false(is_gamma_site_model(h))

  # No gamma_shape_prior_distr
  h <- g[names(g) != "gamma_shape_prior_distr"]
  expect_false(is_gamma_site_model(h))

  # Having a distribution, that is no distribution
  h <- create_gamma_site_model()
  h$gamma_shape_prior_distr <- "nonsense"
  expect_false(is_gamma_site_model(h))
})
