test_that("use", {
  filename <- create_temp_treelog_filename()
  expect_true(!file.exists(filename))
})
