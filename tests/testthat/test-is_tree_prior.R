context("is_tree_prior")

test_that("use", {

  expect_true(is_tree_prior(create_bd_tree_prior()))
  expect_false(is_tree_prior("nonsense"))

  expect_true(is_bd_tree_prior(create_bd_tree_prior()))
  expect_false(is_bd_tree_prior("nonsense"))

  expect_true(is_cbs_tree_prior(create_cbs_tree_prior()))
  expect_false(is_cbs_tree_prior("nonsense"))

  expect_true(is_ccp_tree_prior(create_ccp_tree_prior()))
  expect_false(is_ccp_tree_prior("nonsense"))

  expect_true(is_cep_tree_prior(create_cep_tree_prior()))
  expect_false(is_cep_tree_prior("nonsense"))

  expect_true(is_yule_tree_prior(create_yule_tree_prior()))
  expect_false(is_yule_tree_prior("nonsense"))

})

test_that("is_tree_prior, devious", {

  g <- create_yule_tree_prior()
  expect_true(is_tree_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_tree_prior(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_tree_prior(h))

  # No 'id'
  h <- g[names(g) != "id"]
  expect_false(is_tree_prior(h))
})

test_that("is_bd_tree_prior, devious", {

  g <- create_bd_tree_prior()
  expect_true(is_bd_tree_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_bd_tree_prior(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_bd_tree_prior(h))

  # No 'birth_rate_distr'
  h <- g[names(g) != "birth_rate_distr"]
  expect_false(is_bd_tree_prior(h))

  # No 'death_rate_distr'
  h <- g[names(g) != "death_rate_distr"]
  expect_false(is_bd_tree_prior(h))
})

test_that("is_cbs_tree_prior, devious", {

  g <- create_cbs_tree_prior()
  expect_true(is_cbs_tree_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_cbs_tree_prior(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_cbs_tree_prior(h))

  # No 'group_sizes_dimension'
  h <- g[names(g) != "group_sizes_dimension"]
  expect_false(is_cbs_tree_prior(h))

  # No 'b_pop_sizes_param'
  h <- g[names(g) != "b_pop_sizes_param"]
  expect_false(is_cbs_tree_prior(h))

  # No 'pop_sizes_scaler_scale_factor'
  h <- g[names(g) != "pop_sizes_scaler_scale_factor"]
  expect_false(is_cbs_tree_prior(h))

})

test_that("is_ccp_tree_prior, devious", {

  g <- create_ccp_tree_prior()
  expect_true(is_ccp_tree_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_ccp_tree_prior(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_ccp_tree_prior(h))

  # No 'pop_size_distr'
  h <- g[names(g) != "pop_size_distr"]
  expect_false(is_ccp_tree_prior(h))
})

test_that("is_cep_tree_prior, devious", {

  g <- create_cep_tree_prior()
  expect_true(is_cep_tree_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_cep_tree_prior(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_cep_tree_prior(h))

  # No 'pop_size_distr'
  h <- g[names(g) != "pop_size_distr"]
  expect_false(is_cep_tree_prior(h))

  # No 'growth_rate_distr'
  h <- g[names(g) != "growth_rate_distr"]
  expect_false(is_cep_tree_prior(h))
})

test_that("is_yule_tree_prior, devious", {

  g <- create_yule_tree_prior()
  expect_true(is_yule_tree_prior(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_yule_tree_prior(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_yule_tree_prior(h))

  # No 'birth_rate_distr'
  h <- g[names(g) != "birth_rate_distr"]
  expect_false(is_yule_tree_prior(h))
})
