context("create_tree_prior")

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
