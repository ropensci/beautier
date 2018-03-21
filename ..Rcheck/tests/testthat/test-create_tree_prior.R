context("create_tree_prior")

test_that("use general function", {

  tree_prior <- beautier::create_bd_tree_prior()
  testthat::expect_true(beautier:::is_bd_tree_prior(tree_prior))

  tree_prior <- beautier::create_cbs_tree_prior()
  testthat::expect_true(beautier:::is_cbs_tree_prior(tree_prior))

  tree_prior <- beautier::create_ccp_tree_prior()
  testthat::expect_true(beautier:::is_ccp_tree_prior(tree_prior))

  tree_prior <- beautier::create_cep_tree_prior()
  testthat::expect_true(beautier:::is_cep_tree_prior(tree_prior))

  tree_prior <- beautier::create_yule_tree_prior()
  testthat::expect_true(beautier:::is_yule_tree_prior(tree_prior))

})

test_that("use typesafe alias", {

  tree_prior <- beautier::create_tree_prior_bd()
  testthat::expect_true(beautier:::is_bd_tree_prior(tree_prior))

  tree_prior <- beautier::create_tree_prior_cbs()
  testthat::expect_true(beautier:::is_cbs_tree_prior(tree_prior))

  tree_prior <- beautier::create_tree_prior_ccp()
  testthat::expect_true(beautier:::is_ccp_tree_prior(tree_prior))

  tree_prior <- beautier::create_tree_prior_cep()
  testthat::expect_true(beautier:::is_cep_tree_prior(tree_prior))

  tree_prior <- beautier::create_tree_prior_yule()
  testthat::expect_true(beautier:::is_yule_tree_prior(tree_prior))

})

test_that("use general function with get_tree_prior_names", {

  names <- get_tree_prior_names()
  for (name in names) {
    tree_prior <- beautier:::create_tree_prior(
      name = name,
      id = NA
    )
    testthat::expect_true(is_tree_prior(tree_prior))
  }

})

test_that("abuse", {

  testthat::expect_error(
    beautier:::create_tree_prior(name = "nonsense"),
    "invalid tree prior name"
  )

})
