context("is_cbs_tree_prior")

test_that("use", {

  testthat::expect_true(
    is_cbs_tree_prior(create_cbs_tree_prior()))

  testthat::expect_false(
    is_cbs_tree_prior(create_bd_tree_prior()))

})
