context("is_tree_prior_name")

test_that("basic usage", {

  testthat::expect_false(beautier::is_tree_prior_name("nonsense"))
  testthat::expect_true(beautier::is_tree_prior_name("yule"))
  testthat::expect_true(beautier::is_tree_prior_name("birth_death"))
  testthat::expect_true(
    beautier::is_tree_prior_name("coalescent_constant_population"))

})
