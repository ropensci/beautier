context("are_initialized_tree_priors")

test_that("initialized", {

  init_yule_priors <- list(
    create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1)
    )
  )
  testthat::expect_true(
    are_initialized_tree_priors(init_yule_priors)
  )
})


test_that("uninitialized", {

  uninit_yule_priors <- list(
    create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = NA)
    )
  )

  testthat::expect_false(
    are_initialized_tree_priors(uninit_yule_priors)
  )

})
