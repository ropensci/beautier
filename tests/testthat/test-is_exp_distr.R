test_that("use", {
  expect_true(is_exp_distr(create_exp_distr()))
  expect_false(is_exp_distr("nonsense"))
  expect_false(is_exp_distr(NA))
  expect_false(is_exp_distr(NULL))
  expect_false(is_exp_distr(Inf))
  expect_false(is_exp_distr(c()))
})
