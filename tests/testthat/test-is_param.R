context("is_param")

test_that("use", {

  expect_true(beautier:::is_param(create_alpha_param()))
  expect_true(beautier:::is_param(create_beta_param()))
  expect_true(beautier:::is_param(create_clock_rate_param()))
  expect_true(beautier:::is_param(create_m_param()))
  expect_true(beautier:::is_param(create_mean_param()))
  expect_true(beautier:::is_param(create_mu_param()))
  expect_true(beautier:::is_param(create_kappa_1_param()))
  expect_true(beautier:::is_param(create_s_param()))
  expect_true(beautier:::is_param(create_scale_param()))
  expect_true(beautier:::is_param(create_sigma_param()))

  expect_false(beautier:::is_param(NULL))
  expect_false(beautier:::is_param(NA))
  expect_false(beautier:::is_param(c()))
  expect_false(beautier:::is_param(ape::rcoal(3)))

})
