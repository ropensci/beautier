test_that("use", {
  check_empty_beautier_folder()

  expect_true(
    is_yule_tree_prior(create_yule_tree_prior())
  )
  expect_false(
    is_yule_tree_prior(create_bd_tree_prior())
  )

  check_empty_beautier_folder()
})
