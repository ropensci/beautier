context("is_clock_model")

test_that("use", {

  expect_true(is_clock_model(create_strict_clock_model()))
  expect_true(is_clock_model(create_rln_clock_model()))
  expect_false(is_clock_model("nonsense"))
  expect_false(is_clock_model(NULL))
  expect_false(is_clock_model(NA))
  expect_false(is_clock_model(list(name = "nonsense")))

  expect_true(is_strict_clock_model(create_strict_clock_model()))
  expect_false(is_strict_clock_model(create_rln_clock_model()))
  expect_false(is_strict_clock_model("nonsense"))
  expect_false(is_strict_clock_model(NA))
  expect_false(is_strict_clock_model(NULL))

  expect_true(is_rln_clock_model(create_rln_clock_model()))
  expect_false(is_rln_clock_model(create_strict_clock_model()))
  expect_false(is_rln_clock_model("nonsense"))
  expect_false(is_rln_clock_model(NA))
  expect_false(is_rln_clock_model(NULL))
})

test_that("is_strict_clock_model: devious", {

  g <- create_strict_clock_model()
  expect_true(is_strict_clock_model(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_strict_clock_model(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_strict_clock_model(h))

  # No 'id'
  h <- g[names(g) != "id"]
  expect_false(is_strict_clock_model(h))

  # No 'clock_rate_param'
  h <- g[names(g) != "clock_rate_param"]
  expect_false(is_strict_clock_model(h))

  # Invalid 'clock_rate_param'
  h <- g
  h$clock_rate_param <- "nonsense"
  expect_false(is_strict_clock_model(h))

  # No 'clock_rate_distr'
  h <- g[names(g) != "clock_rate_distr"]
  expect_false(is_strict_clock_model(h))

  # Invalid 'clock_rate_distr'
  h <- g
  h$clock_rate_distr <- "nonsense"
})

test_that("is_rln_clock_model: devious", {

  g <- create_rln_clock_model()
  expect_true(is_rln_clock_model(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_rln_clock_model(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_rln_clock_model(h))

  # No 'id'
  h <- g[names(g) != "id"]
  expect_false(is_rln_clock_model(h))

  # No 'ucldstdev_distr'
  h <- g[names(g) != "ucldstdev_distr"]
  expect_false(is_rln_clock_model(h))

  # No 'mean_rate_prior_distr'
  h <- g[names(g) != "mean_rate_prior_distr"]
  expect_false(is_rln_clock_model(h))

  # No 'mparam_id'
  h <- g[names(g) != "mparam_id"]
  expect_false(is_rln_clock_model(h))

  # No 'mean_clock_rate'
  h <- g[names(g) != "mean_clock_rate"]
  expect_false(is_rln_clock_model(h))

  # No 'n_rate_categories'
  h <- g[names(g) != "n_rate_categories"]
  expect_false(is_rln_clock_model(h))

  # No 'normalize_mean_clock_rate'
  h <- g[names(g) != "normalize_mean_clock_rate"]
  expect_false(is_rln_clock_model(h))

  # No 'dimension'
  h <- g[names(g) != "dimension"]
  expect_false(is_rln_clock_model(h))
})
