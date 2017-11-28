context("is_param")

test_that("use", {

  testthat::expect_true(is_param(create_alpha_param()))
  testthat::expect_true(is_param(create_beta_param()))
  testthat::expect_true(is_param(create_clock_rate_param()))
  testthat::expect_true(is_param(create_m_param()))
  testthat::expect_true(is_param(create_mean_param()))
  testthat::expect_true(is_param(create_mu_param()))
  testthat::expect_true(is_param(create_s_param()))
  testthat::expect_true(is_param(create_scale_param()))
  testthat::expect_true(is_param(create_sigma_param()))

  testthat::expect_false(is_param(NULL))
  testthat::expect_false(is_param(NA))
  testthat::expect_false(is_param(c()))
  testthat::expect_false(is_param(ape::rcoal(3)))

})
