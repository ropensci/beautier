test_that("use", {
  tracelog <- create_test_tracelog()
  expect_equal(tracelog$log_every, 1000)
  expect_true(!file.exists(tracelog$filename))
})
