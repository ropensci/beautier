context("create_param")

test_that("use", {

  testthat::expect_silent(create_alpha_param())
  testthat::expect_silent(create_beta_param())
  testthat::expect_silent(create_clock_rate_param())
  testthat::expect_silent(create_m_param())
  testthat::expect_silent(create_mean_param())
  testthat::expect_silent(create_mu_param())
  testthat::expect_silent(create_s_param())
  testthat::expect_silent(create_scale_param())
  testthat::expect_silent(create_sigma_param())

})

test_that("abuse", {

  testthat::expect_error(
    create_param(name = "nonsense"),
    "invalid parameter name, must be one these:"
  )
})
