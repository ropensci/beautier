context("is_param")

test_that("use", {

  expect_true(is_param(create_alpha_param()))
  expect_true(is_param(create_beta_param()))
  expect_true(is_param(create_clock_rate_param()))
  expect_true(is_param(create_m_param()))
  expect_true(is_param(create_mean_param()))
  expect_true(is_param(create_mu_param()))
  expect_true(is_param(create_kappa_1_param()))
  expect_true(is_param(create_s_param()))
  expect_true(is_param(create_scale_param()))
  expect_true(is_param(create_sigma_param()))

  expect_false(is_param(NULL))
  expect_false(is_param(NA))
  expect_false(is_param(c()))
  expect_false(is_param(ape::rcoal(3)))

})

test_that("is_alpha_param", {

  expect_true(is_alpha_param(create_alpha_param()))
  expect_false(is_alpha_param("nonsense"))

})

test_that("is_beta_param", {

  expect_true(is_beta_param(create_beta_param()))
  expect_false(is_beta_param("nonsense"))

})

test_that("is_clock_rate_param", {

  expect_true(is_clock_rate_param(create_clock_rate_param()))
  expect_false(is_clock_rate_param("nonsense"))

})

test_that("is_kappa_1_param", {

  expect_true(is_kappa_1_param(create_kappa_1_param()))
  expect_false(is_kappa_1_param("nonsense"))

})

test_that("is_kappa_2_param", {

  expect_true(is_kappa_2_param(create_kappa_2_param()))
  expect_false(is_kappa_2_param("nonsense"))

})

test_that("is_lambda_param", {

  expect_true(is_lambda_param(create_lambda_param()))
  expect_false(is_lambda_param("nonsense"))

})

test_that("is_m_param", {

  expect_true(is_m_param(create_m_param()))
  expect_false(is_m_param("nonsense"))

})

test_that("is_mean_param", {

  expect_true(is_mean_param(create_mean_param()))
  expect_false(is_mean_param("nonsense"))

})

test_that("is_mu_param", {

  expect_true(is_mu_param(create_mu_param()))
  expect_false(is_mu_param("nonsense"))

})

test_that("is_rate_ac_param", {

  expect_true(is_rate_ac_param(create_rate_ac_param()))
  expect_false(is_rate_ac_param("nonsense"))

})

test_that("is_rate_ag_param", {

  expect_true(is_rate_ag_param(create_rate_ag_param()))
  expect_false(is_rate_ag_param("nonsense"))

})

test_that("is_rate_at_param", {

  expect_true(is_rate_at_param(create_rate_at_param()))
  expect_false(is_rate_at_param("nonsense"))

})

test_that("is_rate_cg_param", {

  expect_true(is_rate_cg_param(create_rate_cg_param()))
  expect_false(is_rate_cg_param("nonsense"))

})

test_that("is_rate_ct_param", {

  expect_true(is_rate_ct_param(create_rate_ct_param()))
  expect_false(is_rate_ct_param("nonsense"))

})

test_that("is_rate_gt_param", {

  expect_true(is_rate_gt_param(create_rate_gt_param()))
  expect_false(is_rate_gt_param("nonsense"))

})

test_that("is_s_param", {

  expect_true(is_s_param(create_s_param()))
  expect_false(is_s_param("nonsense"))

})

test_that("is_scale_param", {

  expect_true(is_scale_param(create_scale_param()))
  expect_false(is_scale_param("nonsense"))

})

test_that("is_sigma_param", {

  expect_true(is_sigma_param(create_sigma_param()))
  expect_false(is_sigma_param("nonsense"))

})
