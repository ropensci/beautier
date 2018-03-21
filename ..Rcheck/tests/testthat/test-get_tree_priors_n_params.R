context("get_tree_priors_n_params")

test_that("use", {

  tree_prior_0_params <- create_bd_tree_prior()
  tree_prior_1_params <- create_bd_tree_prior(
    birth_rate_distr = create_poisson_distr(), # 1
    death_rate_distr = create_uniform_distr() # 0
  )
  tree_prior_2_params <- create_bd_tree_prior(
    birth_rate_distr = create_poisson_distr(), # 1
    death_rate_distr = create_exp_distr() #1
  )
  tree_prior_3_params <- create_bd_tree_prior(
    birth_rate_distr = create_exp_distr(), # 1
    death_rate_distr = create_laplace_distr() #2
  )
  testit::assert(get_tree_prior_n_params(tree_prior_0_params) == 0)
  testit::assert(get_tree_prior_n_params(tree_prior_1_params) == 1)
  testit::assert(get_tree_prior_n_params(tree_prior_2_params) == 2)
  testit::assert(get_tree_prior_n_params(tree_prior_3_params) == 3)

  testthat::expect_equal(
    get_tree_priors_n_params(
      list(tree_prior_0_params, tree_prior_1_params)
    ),
    1
  )

  testthat::expect_equal(
    get_tree_priors_n_params(
      list(tree_prior_2_params, tree_prior_3_params)
    ),
    5
  )

})


test_that("abuse", {

  testthat::expect_error(
    get_tree_priors_n_params("nonsense"),
    "'tree_priors' must be a list of tree priors"
  )

})
