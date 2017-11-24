context("get_tree_prior_n_params")

test_that("use, BD", {

  # birth_rate_distr: uniform: 0
  # death_rate_distr: uniform: 0
  testthat::expect_equal(get_tree_prior_n_params(create_bd_tree_prior()), 0)

  testthat::expect_equal(get_tree_prior_n_params(
    create_bd_tree_prior(
      birth_rate_distr = create_exp_distr(), # 1
      death_rate_distr = create_laplace_distr() #2
      )
    ),
    3
  )

})

test_that("use, CBS", {

  # no distributions, thus no parameters
  testthat::expect_equal(get_tree_prior_n_params(create_cbs_tree_prior()), 0)

})

test_that("use, CCP", {

  # pop_size_distr: one_div_x_distr: 0
  testthat::expect_equal(get_tree_prior_n_params(create_ccp_tree_prior()), 0)

  testthat::expect_equal(get_tree_prior_n_params(
    create_ccp_tree_prior(
      pop_size_distr = create_exp_distr() # 1
      )
    ),
    1
  )
})

test_that("use, CEP", {

  # pop_size_distr: one_div_x: 0
  # growth_rate_distr: laplace: 2
  testthat::expect_equal(get_tree_prior_n_params(create_cep_tree_prior()), 2)

  testthat::expect_equal(get_tree_prior_n_params(
    create_cep_tree_prior(
      pop_size_distr = create_exp_distr(), # 1
      growth_rate_distr = create_laplace_distr() #2
      )
    ),
    3
  )
})

test_that("use, Yule", {

  # birth_rate_distr: uniform: 0
  testthat::expect_equal(get_tree_prior_n_params(create_yule_tree_prior()), 0)

  testthat::expect_equal(get_tree_prior_n_params(
    create_yule_tree_prior(
      birth_rate_distr = create_laplace_distr() #2
      )
    ),
    2
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_tree_prior_n_params("nonsense"),
    "'tree_prior' must be a tree prior"
  )
  testthat::expect_error(
    get_tree_prior_n_params(NA),
    "'tree_prior' must be a tree prior"
  )
  testthat::expect_error(
    get_tree_prior_n_params(NULL),
    "'tree_prior' must be a tree prior"
  )
  testthat::expect_error(
    get_tree_prior_n_params(c()),
    "'tree_prior' must be a tree prior"
  )

})
