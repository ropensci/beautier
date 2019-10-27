test_that("use", {
  filename <- create_temp_tracelog_filename()
  expect_true(!file.exists(filename))
})
