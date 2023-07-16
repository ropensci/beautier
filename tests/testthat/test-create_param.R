test_that("use, default values", {

  # English name
  expect_silent(create_alpha_param())
  expect_silent(create_b_pop_sizes_param())
  expect_silent(create_beta_param())
  expect_silent(create_clock_rate_param())
  expect_silent(create_kappa_param())
  expect_silent(create_kappa_1_param())
  expect_silent(create_kappa_2_param())
  expect_silent(create_lambda_param())
  expect_silent(create_mean_param())
  expect_silent(create_mu_param())
  expect_silent(create_rate_ac_param())
  expect_silent(create_rate_ag_param())
  expect_silent(create_rate_at_param())
  expect_silent(create_rate_cg_param())
  expect_silent(create_rate_ct_param())
  expect_silent(create_rate_gt_param())
  expect_silent(create_s_param())
  expect_silent(create_scale_param())
  expect_silent(create_sigma_param())

  # Searchable name
  expect_silent(create_param_alpha())
  expect_silent(create_param_b_pop_sizes())
  expect_silent(create_param_beta())
  expect_silent(create_param_clock_rate())
  expect_silent(create_param_kappa())
  expect_silent(create_param_kappa_1())
  expect_silent(create_param_kappa_2())
  expect_silent(create_param_lambda())
  expect_silent(create_param_mean())
  expect_silent(create_param_mu())
  expect_silent(create_param_rate_ac())
  expect_silent(create_param_rate_ag())
  expect_silent(create_param_rate_at())
  expect_silent(create_param_rate_cg())
  expect_silent(create_param_rate_ct())
  expect_silent(create_param_rate_gt())
  expect_silent(create_param_s())
  expect_silent(create_param_scale())
  expect_silent(create_param_sigma())

})

test_that("use, valid function arguments", {

  # English name
  expect_silent(
    create_alpha_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_b_pop_sizes_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_beta_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_clock_rate_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_kappa_param(id = 1, lower = 0.1, value = 0.5)
  )
  expect_silent(
    create_kappa_1_param(id = 1, lower = 0.1, value = 0.5)
  )
  expect_silent(
    create_kappa_2_param(id = 1, lower = 0.1, value = 0.5)
  )
  expect_silent(
    create_lambda_param(id = 1, value = 1.0)
  )
  expect_silent(
    create_mean_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_mu_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_rate_ac_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_rate_ag_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_rate_at_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_rate_cg_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_rate_ct_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_rate_gt_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_s_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_scale_param(id = 1, value = 0.5)
  )
  expect_silent(
    create_sigma_param(id = 1, value = 0.5)
  )

  # Searchable name
  expect_silent(
    create_param_alpha(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_beta(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_clock_rate(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_kappa(id = 1, lower = 0.1, value = 0.5)
  )
  expect_silent(
    create_param_kappa_1(id = 1, lower = 0.1, value = 0.5)
  )
  expect_silent(
    create_param_kappa_2(id = 1, lower = 0.1, value = 0.5)
  )
  expect_silent(
    create_param_lambda(id = 1, value = 1.0)
  )
  expect_silent(
    create_param_mean(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_mu(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_rate_ac(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_rate_ag(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_rate_at(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_rate_cg(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_rate_ct(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_rate_gt(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_s(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_scale(id = 1, value = 0.5)
  )
  expect_silent(
    create_param_sigma(id = 1, value = 0.5)
  )

})

test_that("abuse", {

  expect_error(
    create_param(name = "nonsense"),
    "invalid parameter name, must be one these:"
  )
})

test_that("abuse, create_s_param", {
  # https://github.com/ropensci/beautier/issues/46
  # creating a TN93 model with the default settings
  # creates two log-normal distributions
  # as priors for kappa1 and kappa2,
  # where the S parameter has lower=0, upper=0 and value=1.25.
  # This works if S is not estimated,
  # because BEAST2 only checks upper and lower
  # when modifying the value (not when initialising it)
  # but it will break if S is estimated.
  if (1 == 2) {
    # S param does not facilitate a hyper parameter yet
    expect_silent(
      create_s_param(estimate = FALSE, lower = 0.0, upper = Inf, value = 1.25)
    )
    expect_error(
      create_s_param(estimate = TRUE, lower = 2.0, upper = 1.0, value = 1.5),
      "'lower' must be less than 'upper' when S is estimated"
    )
    expect_error(
      create_s_param(estimate = TRUE, lower = 0.0, upper = 1.0, value = 1.25),
      "'value' must be between 'lower' and 'upper' when S is estimated"
    )
  }
})
