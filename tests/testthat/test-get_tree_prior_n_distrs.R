context("get_tree_prior_n_distrs")

test_that("use", {

  # birth_rate_distr and death_rate_distr
  testthat::expect_equal(get_tree_prior_n_distrs(create_bd_tree_prior()), 2)

  # none
  testthat::expect_equal(get_tree_prior_n_distrs(create_cbs_tree_prior()), 0)

  # pop_size_distr
  testthat::expect_equal(get_tree_prior_n_distrs(create_ccp_tree_prior()), 1)

  # pop_size_distr and growth_rate_distr
  testthat::expect_equal(get_tree_prior_n_distrs(create_cep_tree_prior()), 2)

  # birth_rate_distr
  testthat::expect_equal(get_tree_prior_n_distrs(create_yule_tree_prior()), 1)

})

test_that("abuse", {

  testthat::expect_error(
    get_tree_prior_n_distrs("nonsense"),
    "'tree_prior' must be a tree prior"
  )
  testthat::expect_error(
    get_tree_prior_n_distrs(NA),
    "'tree_prior' must be a tree prior"
  )
  testthat::expect_error(
    get_tree_prior_n_distrs(NULL),
    "'tree_prior' must be a tree prior"
  )
  testthat::expect_error(
    get_tree_prior_n_distrs(c()),
    "'tree_prior' must be a tree prior"
  )

})
