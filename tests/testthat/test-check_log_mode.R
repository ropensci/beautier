test_that("use", {
  expect_silent(check_log_mode("tree"))
  expect_silent(check_log_mode("compound"))
  expect_silent(check_log_mode("autodetect"))

  expect_error(check_log_mode("nonsense"))
  expect_error(check_log_mode(NA))
  expect_error(check_log_mode(NULL))
  expect_error(check_log_mode(Inf))
})
