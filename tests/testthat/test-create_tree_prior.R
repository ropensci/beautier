context("create_tree_prior")

test_that("use general function", {

  tree_prior <- beastscriptr::create_tree_prior(name = "birth_death")
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

  tree_prior <- beastscriptr::create_tree_prior(
    name = "coalescent_bayesian_skyline")
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

  tree_prior <- beastscriptr::create_tree_prior(
    name = "coalescent_constant_population")
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

  tree_prior <- beastscriptr::create_tree_prior(name = "yule")
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

})

test_that("use typesafe alias", {

  tree_prior <- beastscriptr::create_yule_tree_prior()
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

  tree_prior <- beastscriptr::create_bd_tree_prior()
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

  tree_prior <- beastscriptr::create_cbs_tree_prior()
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

  tree_prior <- beastscriptr::create_ccp_tree_prior()
  testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))

})

test_that("use general function with get_tree_prior_names", {

  names <- get_tree_prior_names()
  for (name in names) {
    tree_prior <- beastscriptr::create_tree_prior(name = name)
    testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))
  }

})
