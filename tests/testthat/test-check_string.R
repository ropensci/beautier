test_that("use", {
  expect_silent(check_string("A string :-)"))
  expect_silent(check_string(""))
  expect_error(check_string(c()))
  expect_error(check_string(NULL))
  expect_error(check_string(NA))
  expect_error(check_string(Inf))
  expect_error(check_string(c(TRUE, TRUE)))
  expect_error(check_string(c("one", "too many")), "must be a single string")

  expect_silent(check_string(NA, allow_na = TRUE))
  expect_error(check_string(NA, allow_na = FALSE))

  expect_silent(check_string("", allow_empty = TRUE))
  expect_error(check_string("", allow_empty = FALSE))

})
