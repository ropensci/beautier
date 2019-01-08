context("test-create_tree_prior_from_name")

test_that("use", {
  names <- get_tree_prior_names()
  for (name in names) {
    expect_equal(name, create_tree_prior_from_name(name)$name)
  }
})
