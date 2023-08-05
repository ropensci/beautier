test_that("basic usage", {

  expect_false(is_tree_prior_name("nonsense"))
  expect_true(is_tree_prior_name("yule"))
  expect_true(is_tree_prior_name("birth_death"))
  expect_true(
    is_tree_prior_name("coalescent_constant_population")
  )
})
