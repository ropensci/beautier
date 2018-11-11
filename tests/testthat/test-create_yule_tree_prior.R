context("create_yule_tree_prior")

test_that("use", {

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_prior()
    )
  )

})
