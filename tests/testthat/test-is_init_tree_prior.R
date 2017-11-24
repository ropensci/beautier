context("is_init_tree_prior")

test_that("use", {

  testthat::expect_false(
    beautier:::is_init_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_exp_distr(id = 1)
      )
    )
  )

  testthat::expect_true(
    is_init_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      )
    )
  )

  testthat::expect_false(
    is_init_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = NA)
      )
    )
  )

})
