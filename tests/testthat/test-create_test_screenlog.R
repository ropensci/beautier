test_that("use", {
  screenlog <- create_test_screenlog()
  expect_equal(screenlog$log_every, 1000)
  expect_true(!file.exists(screenlog$filename))
})
