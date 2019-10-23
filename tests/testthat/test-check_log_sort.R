test_that("use", {
  expect_silent(check_log_sort("alphabetic"))
  expect_silent(check_log_sort("none"))
  expect_silent(check_log_sort("smart"))

  expect_error(check_log_sort("nonsense"))
  expect_error(check_log_sort(NA))
  expect_error(check_log_sort(NULL))
  expect_error(check_log_sort(Inf))
})
