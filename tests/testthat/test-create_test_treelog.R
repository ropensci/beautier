test_that("use", {
  treelog <- create_test_treelog()
  expect_equal(treelog$log_every, 1000)
  expect_true(!file.exists(treelog$filename))
})
