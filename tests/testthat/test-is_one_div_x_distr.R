context("is_one_div_x_distr")

test_that("use", {

  expect_true(is_one_div_x_distr(create_one_div_x_distr()))
  expect_false(is_one_div_x_distr("nonsense"))
  expect_false(is_one_div_x_distr(is_one_div_x_distr))
  expect_false(is_one_div_x_distr(42))
  expect_false(is_one_div_x_distr(NA))
  expect_false(is_one_div_x_distr(NULL))
})
