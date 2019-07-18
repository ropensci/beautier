test_that("use", {
  expect_true(is_one_string("hello"))
  expect_true(is_one_string("hello world"))
  expect_false(is_one_string(NULL))
  expect_false(is_one_string(NA))
  expect_false(is_one_string(TRUE))
  expect_false(is_one_string(c()))
  expect_false(is_one_string(c("hello", "world")))
})
