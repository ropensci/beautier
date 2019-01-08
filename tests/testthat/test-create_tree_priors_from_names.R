context("test-create_tree_priors_from_names")

test_that("use", {

  names <- get_tree_prior_names()
  tree_priors <- create_tree_priors_from_names(names)
  # Indexed use
  for (i in seq_along(names)) {
    expect_equal(names[i], tree_priors[[i]]$name)
  }
})
