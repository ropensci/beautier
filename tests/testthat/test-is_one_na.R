context("is_one_na")

test_that("use", {

  expect_true(is_one_na(NA))
  expect_false(is_one_na(NULL))
  expect_false(is_one_na(c(NA, NA)))
  expect_false(is_one_na(c(NULL, NULL)))
  expect_false(is_one_na("nonsense"))
  expect_false(is_one_na(is_one_na))
  expect_false(is_one_na(314))
  expect_false(is_one_na(c(3, 1, 4)))
  expect_false(is_one_na(Inf))
})
