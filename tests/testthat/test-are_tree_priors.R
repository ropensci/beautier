context("are_tree_priors")

test_that("use", {

  expect_true(are_tree_priors(create_yule_tree_prior(id = "some_id")))
  expect_false(are_tree_priors("nonsense"))
  expect_false(are_tree_priors(rep("nonsense", 2)))
  expect_false(are_tree_priors(NA))
  expect_false(are_tree_priors(NULL))
})
