context("test-check_tree_prior")

test_that("use", {
  expect_silent(check_tree_prior(create_yule_tree_prior()))
  expect_silent(check_tree_prior(create_bd_tree_prior()))
  expect_silent(check_tree_prior(create_cbs_tree_prior()))
  expect_silent(check_tree_prior(create_ccp_tree_prior()))
  expect_silent(check_tree_prior(create_cep_tree_prior()))

  # Can use lists
  expect_silent(check_tree_prior(list(create_yule_tree_prior())))

  # Must be one tree prior
  expect_error(
    check_tree_prior(list(create_yule_tree_prior(), create_yule_tree_prior())),
    "'tree_prior' must be a valid tree prior"
  )

  # Must be a tree prior
  expect_error(
    check_tree_prior("nonsense"),
    "'tree_prior' must be a valid tree prior"
  )
  expect_error(
    check_tree_prior(NULL),
    "'tree_prior' must be a valid tree prior"
  )
  expect_error(
    check_tree_prior(NA),
    "'tree_prior' must be a valid tree prior"
  )
})
