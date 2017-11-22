context("init_tree_priors")

test_that("re-initialize", {

  tree_priors_before <- list(
    create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  testit::assert(are_init_tree_priors(tree_priors_before))
  tree_priors_after <- init_tree_priors(tree_priors = tree_priors_before)
  testthat::expect_true(are_init_tree_priors(tree_priors_after))

})

test_that("initialize BD prior", {

  before <- list(create_bd_tree_prior())
  testit::assert(!are_init_tree_priors(before))
  after <- init_tree_priors(before)
  testthat::expect_true(are_init_tree_priors(after))

})

test_that("initialize CBS prior", {

  before <- list(create_cbs_tree_prior())
  # CBS is always initialized
  testit::assert(are_init_tree_priors(before))
  after <- init_tree_priors(before)
  testthat::expect_true(are_init_tree_priors(after))

})

test_that("initialize CCP prior", {

  before <- list(create_ccp_tree_prior())
  testit::assert(!are_init_tree_priors(before))
  after <- init_tree_priors(before)
  testthat::expect_true(are_init_tree_priors(after))

})

test_that("initialize CEP prior", {

  before <- list(create_cep_tree_prior())
  testit::assert(!beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before)
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})

test_that("initialize Yule prior", {

  before <- list(create_yule_tree_prior())
  testit::assert(!beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before)
  testthat::expect_true(beautier:::are_init_tree_priors(after))

  before <- list(create_yule_tree_prior(
  birth_rate_distr = create_exponential_distr(id = 1)))
  testit::assert(!beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before)
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})
