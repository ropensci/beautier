test_that("all tree_priors must be recognized as such", {

  tree_priors <- create_tree_priors()
  expect_true(length(tree_priors) > 1)
  for (tree_prior in tree_priors) {
    expect_true(is_tree_prior(tree_prior))
  }
})
