context("init_tree_priors")

test_that("re-initialize", {

  before <- list(
    create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  testit::assert(beautier:::are_init_tree_priors(before))
  testit::assert(beautier:::is_yule_tree_prior(before))
  after <- beautier:::init_tree_priors(tree_priors = before, ids = "some_id")
  testthat::expect_true(beautier:::is_yule_tree_prior(after))
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})

test_that("initialize BD prior", {

  before <- list(create_bd_tree_prior())
  testit::assert(!beautier:::are_init_tree_priors(before))
  testit::assert(beautier:::is_bd_tree_prior(before[[1]]))
  after <- beautier:::init_tree_priors(before, ids = "some_id")
  testthat::expect_true(beautier:::is_bd_tree_prior(after[[1]]))
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})

test_that("initialize CBS prior", {

  before <- list(create_cbs_tree_prior())
  # CBS is always initialized
  testit::assert(beautier:::is_cbs_tree_prior(before[[1]]))
  testit::assert(beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before, ids = "some_id")
  testthat::expect_true(beautier:::is_cbs_tree_prior(after[[1]]))
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})

test_that("initialize CCP prior", {

  before <- list(create_ccp_tree_prior())
  testit::assert(beautier:::is_ccp_tree_prior(before[[1]]))
  testit::assert(!beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before, ids = "some_id")
  testthat::expect_true(beautier:::is_ccp_tree_prior(after[[1]]))
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})

test_that("initialize CEP prior", {

  before <- list(create_cep_tree_prior())
  testit::assert(beautier:::is_cep_tree_prior(before[[1]]))
  testit::assert(!beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before, ids = "some_id")
  testthat::expect_true(is_cep_tree_prior(after[[1]]))
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})

test_that("initialize Yule prior", {

  before <- list(create_yule_tree_prior())
  testit::assert(beautier:::is_yule_tree_prior(before[[1]]))
  testit::assert(!beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before, ids = "some_id")
  testit::assert(beautier:::is_yule_tree_prior(after[[1]]))
  testthat::expect_true(beautier:::are_init_tree_priors(after))

  before <- list(create_yule_tree_prior(
  birth_rate_distr = create_exp_distr(id = 1)))
  testit::assert(beautier:::is_yule_tree_prior(before[[1]]))
  testit::assert(!beautier:::are_init_tree_priors(before))
  after <- beautier:::init_tree_priors(before, ids = "some_id")
  testit::assert(beautier:::is_yule_tree_prior(after[[1]]))
  testthat::expect_true(beautier:::are_init_tree_priors(after))

})
