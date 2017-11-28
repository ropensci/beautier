context("create_yule_tree_prior")

test_that("use", {

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_prior()
    )
  )

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_priors(ids = "some_id")
    )
  )

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_priors(ids = c("a", "b"))[[1]]
    )
  )

  testthat::expect_true(
    is_yule_tree_prior(
      create_yule_tree_priors(ids = c("a", "b"))[[2]]
    )
  )

})
