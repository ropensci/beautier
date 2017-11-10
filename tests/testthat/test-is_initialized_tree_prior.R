context("is_initialized_tree_prior")

test_that("use", {

  testthat::expect_true(
    is_initialized_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      )
    )
  )

  testthat::expect_false(
    is_initialized_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = NA)
      )
    )
  )

})
