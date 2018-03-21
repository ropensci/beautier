context("get_tree_priors_n_distrs")

test_that("use", {

  testit::assert(get_tree_prior_n_distrs(create_bd_tree_prior()) == 2)
  testit::assert(get_tree_prior_n_distrs(create_cbs_tree_prior()) == 0)
  testit::assert(get_tree_prior_n_distrs(create_ccp_tree_prior()) == 1)
  testit::assert(get_tree_prior_n_distrs(create_cep_tree_prior()) == 2)
  testit::assert(get_tree_prior_n_distrs(create_yule_tree_prior()) == 1)

  testthat::expect_equal(
    get_tree_priors_n_distrs(
      list(
        create_bd_tree_prior(), # has two
        create_ccp_tree_prior() # has one
      )
    ),
    3
  )

  testthat::expect_equal(
    get_tree_priors_n_distrs(
      list(
        create_ccp_tree_prior(), # has one
        create_yule_tree_prior() # has one
      )
    ),
    2
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_tree_priors_n_distrs("nonsense"),
    "'tree_priors' must be a list of one or more tree priors"
  )
})
