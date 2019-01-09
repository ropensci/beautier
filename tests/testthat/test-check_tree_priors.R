context("test-check_tree_priors")

test_that("use", {
  expect_silent(
    check_tree_priors(
      create_yule_tree_prior()
    )
  )
  expect_silent(
    check_tree_priors(
      list(create_yule_tree_prior())
    )
  )
  expect_silent(
    check_tree_priors(
      list(create_yule_tree_prior(), create_bd_tree_prior())
    )
  )
  expect_error(
    check_tree_priors("nonsense"),
    "'tree_priors' must be a list of one or more valid tree priors"
  )
  expect_error(
    check_tree_priors(NULL),
    "'tree_priors' must be a list of one or more valid tree priors"
  )
  expect_error(
    check_tree_priors(NA),
    "'tree_priors' must be a list of one or more valid tree priors"
  )
})
