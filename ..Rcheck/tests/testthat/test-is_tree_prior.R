context("is_tree_prior")

test_that("use", {

  testthat::expect_true(is_tree_prior(create_bd_tree_prior()))
  testthat::expect_false(is_tree_prior("nonsense"))
  testthat::expect_false(is_tree_prior(list(name = "nonsense")))
})
