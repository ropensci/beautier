context("create_yule_tree_prior")

test_that("use", {

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_prior()
    )
  )

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_priors(n = 1)
    )
  )

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_priors(n = 2)[[1]]
    )
  )

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_priors(n = 2)[[2]]
    )
  )

})
