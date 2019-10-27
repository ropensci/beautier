test_that("use", {
  filename <- create_temp_screenlog_filename()
  expect_true(!file.exists(filename))
})
