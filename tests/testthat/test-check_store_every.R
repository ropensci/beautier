test_that("use", {
  expect_silent(check_store_every(-1))
  expect_silent(check_store_every(NA))
  expect_silent(check_store_every(1))

  expect_error(check_store_every(-2))
  expect_error(check_store_every(0))
  expect_error(check_store_every(NULL))
  expect_error(check_store_every(Inf))
})
