context("initialize_tree_priors")

test_that("re-initialize", {

  tree_priors_before <- list(
    create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1)
    )
  )
  testit::assert(are_initialized_tree_priors(tree_priors_before))
  tree_priors_after <- initialize_tree_priors(tree_priors = tree_priors_before)
  testthat::expect_true(are_initialized_tree_priors(tree_priors_after))

})

test_that("initialize BD prior", {

  before <- list(create_bd_tree_prior())
  testit::assert(!are_initialized_tree_priors(before))
  after <- initialize_tree_priors(before)
  testthat::expect_true(are_initialized_tree_priors(after))

})

test_that("initialize CBS prior", {

  before <- list(create_cbs_tree_prior())
  # CBS is always initialized
  testit::assert(are_initialized_tree_priors(before))
  after <- initialize_tree_priors(before)
  testthat::expect_true(are_initialized_tree_priors(after))

})

test_that("initialize CCP prior", {

  skip("WIP")

  before <- list(create_ccp_tree_prior())
  testit::assert(!are_initialized_tree_priors(before))
  after <- initialize_tree_priors(before)
  testthat::expect_true(are_initialized_tree_priors(after))

})

test_that("initialize CEP prior", {

  skip("WIP")

  before <- list(create_cep_tree_prior())
  testit::assert(!are_initialized_tree_priors(before))
  after <- initialize_tree_priors(before)
  testthat::expect_true(are_initialized_tree_priors(after))

})

test_that("initialize Yule prior", {

  before <- list(create_yule_tree_prior())
  testit::assert(!are_initialized_tree_priors(before))
  after <- initialize_tree_priors(before)
  testit::assert(are_initialized_tree_priors(after))

})
