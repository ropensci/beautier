context("create_tree_priors")

test_that("all tree_priors must be recognized as such", {

  tree_priors <- beastscriptr::create_tree_priors()
  testthat::expect_true(length(tree_priors) > 1)
  for (tree_prior in tree_priors) {
    testthat::expect_true(beastscriptr::is_tree_prior(tree_prior))
  }

})

