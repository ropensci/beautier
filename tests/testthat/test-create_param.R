context("create_param")

test_that("use, default values", {

  # English name
  testthat::expect_silent(create_alpha_param())
  testthat::expect_silent(create_beta_param())
  testthat::expect_silent(create_clock_rate_param())
  testthat::expect_silent(create_kappa_1_param())
  testthat::expect_silent(create_kappa_2_param())
  testthat::expect_silent(create_lambda_param())
  testthat::expect_silent(create_m_param())
  testthat::expect_silent(create_mean_param())
  testthat::expect_silent(create_mu_param())
  testthat::expect_silent(create_rate_ac_param())
  testthat::expect_silent(create_rate_ag_param())
  testthat::expect_silent(create_rate_at_param())
  testthat::expect_silent(create_rate_cg_param())
  testthat::expect_silent(create_rate_ct_param())
  testthat::expect_silent(create_rate_gt_param())
  testthat::expect_silent(create_s_param())
  testthat::expect_silent(create_scale_param())
  testthat::expect_silent(create_sigma_param())

  # Searchable name
  testthat::expect_silent(create_param_alpha())
  testthat::expect_silent(create_param_beta())
  testthat::expect_silent(create_param_clock_rate())
  testthat::expect_silent(create_param_kappa_1())
  testthat::expect_silent(create_param_kappa_2())
  testthat::expect_silent(create_param_lambda())
  testthat::expect_silent(create_param_m())
  testthat::expect_silent(create_param_mean())
  testthat::expect_silent(create_param_mu())
  testthat::expect_silent(create_param_rate_ac())
  testthat::expect_silent(create_param_rate_ag())
  testthat::expect_silent(create_param_rate_at())
  testthat::expect_silent(create_param_rate_cg())
  testthat::expect_silent(create_param_rate_ct())
  testthat::expect_silent(create_param_rate_gt())
  testthat::expect_silent(create_param_s())
  testthat::expect_silent(create_param_scale())
  testthat::expect_silent(create_param_sigma())

})

test_that("use, valid function arguments", {

  # English name
  testthat::expect_silent(
    create_alpha_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_beta_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_clock_rate_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_kappa_1_param(id = 1, lower = 0.1, value = 0.5)
  )
  testthat::expect_silent(
    create_kappa_2_param(id = 1, lower = 0.1, value = 0.5)
  )
  testthat::expect_silent(
    create_lambda_param(id = 1, value = 1.0)
  )
  testthat::expect_silent(
    create_m_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_mean_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_mu_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_rate_ac_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_rate_ag_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_rate_at_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_rate_cg_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_rate_ct_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_rate_gt_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_s_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_scale_param(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_sigma_param(id = 1, estimate = TRUE, value = 0.5)
  )

  # Searchable name
  testthat::expect_silent(
    create_param_alpha(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_beta(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_clock_rate(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_kappa_1(id = 1, lower = 0.1, value = 0.5)
  )
  testthat::expect_silent(
    create_param_kappa_2(id = 1, lower = 0.1, value = 0.5)
  )
  testthat::expect_silent(
    create_param_lambda(id = 1, value = 1.0)
  )
  testthat::expect_silent(
    create_param_m(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_mean(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_mu(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_rate_ac(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_rate_ag(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_rate_at(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_rate_cg(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_rate_ct(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_rate_gt(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_s(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_scale(id = 1, estimate = TRUE, value = 0.5)
  )
  testthat::expect_silent(
    create_param_sigma(id = 1, estimate = TRUE, value = 0.5)
  )

})

test_that("abuse", {

  testthat::expect_error(
    create_param(name = "nonsense"),
    "invalid parameter name, must be one these:"
  )
})
