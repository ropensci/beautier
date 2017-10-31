context("create_tree_prior")

test_that("use general function", {

  tree_prior <- beautier::create_tree_prior(name = "birth_death")
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

  tree_prior <- beautier::create_tree_prior(
    name = "coalescent_bayesian_skyline")
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

  tree_prior <- beautier::create_tree_prior(
    name = "coalescent_constant_population")
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

  tree_prior <- beautier::create_tree_prior(name = "yule")
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

})

test_that("use typesafe alias", {

  tree_prior <- beautier::create_yule_tree_prior()
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

  tree_prior <- beautier::create_bd_tree_prior()
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

  tree_prior <- beautier::create_cbs_tree_prior()
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

  tree_prior <- beautier::create_ccp_tree_prior()
  testthat::expect_true(beautier::is_tree_prior(tree_prior))

})

test_that("use general function with get_tree_prior_names", {

  names <- get_tree_prior_names()
  for (name in names) {
    tree_prior <- beautier::create_tree_prior(name = name)
    testthat::expect_true(beautier::is_tree_prior(tree_prior))
  }

})
