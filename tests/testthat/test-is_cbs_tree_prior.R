test_that("use", {
  expect_true(
    is_cbs_tree_prior(create_cbs_tree_prior())
  )
  expect_false(
    is_cbs_tree_prior(create_bd_tree_prior())
  )
})
