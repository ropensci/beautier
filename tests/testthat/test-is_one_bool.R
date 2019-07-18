test_that("use", {

  expect_true(is_one_bool(TRUE))
  expect_true(is_one_bool(FALSE))
  expect_false(is_one_bool(NULL))
  expect_false(is_one_bool(NA))
  expect_false(is_one_bool(c()))
  expect_false(is_one_bool("nonsense"))
  expect_false(is_one_bool(c(TRUE, FALSE)))
})
