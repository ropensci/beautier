test_that("use", {
  expect_silent(check_true(TRUE))
  expect_error(check_true(FALSE))
  expect_error(check_true(c()))
  expect_error(check_true(NULL))
  expect_error(check_true(NA))
  expect_error(check_true(Inf))
  expect_error(check_true(c(TRUE, TRUE)))
})

test_that("use", {
  expect_silent(check_false(FALSE))
  expect_error(check_false(TRUE))
  expect_error(check_false(c()))
  expect_error(check_false(NULL))
  expect_error(check_false(NA))
  expect_error(check_false(Inf))
  expect_error(check_false(c(FALSE, FALSE)))
})
