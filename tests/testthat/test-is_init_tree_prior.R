context("is_init_tree_prior")

test_that("use", {

  expect_false(
    is_init_tree_prior(
      "nonsense"
    )
  )

  expect_false(
    is_init_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_exp_distr(id = 1)
      )
    )
  )

  expect_true(
    is_init_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      )
    )
  )

  expect_false(
    is_init_tree_prior(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = NA)
      )
    )
  )

})
