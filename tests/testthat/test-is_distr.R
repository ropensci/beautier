context("is_distr")

test_that("use", {

  expect_true(is_distr(create_beta_distr()))
  expect_true(is_distr(create_exp_distr()))
  expect_true(is_distr(create_gamma_distr()))
  expect_true(is_distr(create_inv_gamma_distr()))
  expect_true(is_distr(create_laplace_distr()))
  expect_true(is_distr(create_log_normal_distr()))
  expect_true(is_distr(create_normal_distr()))
  expect_true(is_distr(create_one_div_x_distr()))
  expect_true(is_distr(create_poisson_distr()))
  expect_true(is_distr(create_uniform_distr()))
  expect_false(is_distr(list(name = "nonsense")))

})
