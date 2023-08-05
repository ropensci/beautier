test_that("use", {
  expect_true(is_one_empty_string(""))

  # FALSE
  expect_false(is_one_empty_string("3.14"))
  expect_false(is_one_empty_string(c("", "")))
  expect_false(is_one_empty_string(42))
  expect_false(is_one_empty_string("nonsense"))
  expect_false(is_one_empty_string(NA))
  expect_false(is_one_empty_string(NULL))
  expect_false(is_one_empty_string(Inf))
})
