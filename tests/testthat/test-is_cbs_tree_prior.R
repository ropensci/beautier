context("is_cbs_tree_prior")

test_that("use", {

  testthat::expect_true(
    is_cbs_tree_prior(create_tree_prior(name = "coalescent_bayesian_skyline")))

  testthat::expect_false(
    is_cbs_tree_prior(create_tree_prior(name = "birth_death")))

})
