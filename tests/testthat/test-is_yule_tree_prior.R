context("is_yule_tree_prior")

test_that("use", {

  testthat::expect_true(
    is_yule_tree_prior(create_yule_tree_prior()))
  testthat::expect_false(
    is_yule_tree_prior(create_tree_prior(name = "birth_death")))
})
