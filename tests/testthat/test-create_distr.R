context("create_distr")

test_that("use, default", {

  # English function names
  expect_silent(create_beta_distr())
  expect_silent(create_exp_distr())
  expect_silent(create_gamma_distr())
  expect_silent(create_inv_gamma_distr())
  expect_silent(create_laplace_distr())
  expect_silent(create_log_normal_distr())
  expect_silent(create_normal_distr())
  expect_silent(create_one_div_x_distr())
  expect_silent(create_poisson_distr())
  expect_silent(create_uniform_distr())

  # Search-tree friendly function names
  expect_silent(create_distr_beta())
  expect_silent(create_distr_exp())
  expect_silent(create_distr_gamma())
  expect_silent(create_distr_inv_gamma())
  expect_silent(create_distr_laplace())
  expect_silent(create_distr_log_normal())
  expect_silent(create_distr_normal())
  expect_silent(create_distr_one_div_x())
  expect_silent(create_distr_poisson())
  expect_silent(create_distr_uniform())

})

test_that("use, parameters", {

  # English function names
  expect_silent(
    create_beta_distr(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  expect_silent(
    create_exp_distr(id = 1, mean = create_mean_param())
  )
  expect_silent(
    create_gamma_distr(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  expect_silent(
    create_inv_gamma_distr(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  expect_silent(
    create_laplace_distr(
      id = 1, mu = create_mu_param(), scale = create_scale_param()
    )
  )
  expect_silent(
    create_log_normal_distr(id = 1, m = create_m_param(), s = create_s_param())
  )
  expect_silent(
    create_normal_distr(
      id = 1, mean = create_mean_param(), sigma = create_sigma_param()
    )
  )
  expect_silent(
    create_one_div_x_distr(id = 1)
  )
  expect_silent(
    create_poisson_distr(id = 1, lambda = create_lambda_param())
  )
  expect_silent(
    create_uniform_distr()
  )

  # Search-tree friendly function names
  expect_silent(
    create_distr_beta(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  expect_silent(
    create_distr_exp(
      id = 1, mean = create_mean_param()
    )
  )
  expect_silent(
    create_distr_gamma(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  expect_silent(
    create_distr_inv_gamma(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  expect_silent(
    create_distr_laplace(
      id = 1, mu = create_mu_param(), scale = create_scale_param()
    )
  )
  expect_silent(
    create_distr_log_normal(
      id = 1, m = create_m_param(), s = create_s_param()
    )
  )
  expect_silent(
    create_distr_normal(
      id = 1, mean = create_mean_param(), sigma = create_sigma_param()
    )
  )
  expect_silent(
    create_distr_one_div_x(id = 1)
  )
  expect_silent(
    create_distr_poisson(id = 1, lambda = create_lambda_param())
  )
  expect_silent(create_distr_uniform())

})


test_that("use, simpler interface, Issue #71", {

  expect_silent(
    create_beta_distr(
      id = 1,
      alpha = 1.2,
      beta = 2.3
    )
  )
  expect_silent(
    create_exp_distr(
      id = 1,
      mean = 1.0
    )
  )
  expect_silent(
    create_gamma_distr(
      id = 1,
      alpha = 1.2,
      beta = 2.3
    )
  )
  expect_silent(
    create_inv_gamma_distr(
      id = 1,
      alpha = 1.2,
      beta = 2.3
    )
  )
  expect_silent(
    create_laplace_distr(
      id = 1,
      mu = 1.2,
      scale = 2.3
    )
  )
  expect_silent(
    create_log_normal_distr(
      id = 1,
      m = 1.2,
      s = 2.3
    )
  )
  expect_silent(
    create_normal_distr(
      id = 1,
      mean = 1.2,
      sigma = 2.3
    )
  )
  expect_silent(
    create_poisson_distr(
      id = 1,
      lambda = 1.2
    )
  )
})

test_that("abuse", {

  expect_error(
    create_distr(name = "nonsense"),
    "'name' must be a distribution name"
  )

})

test_that("abuse, beta_distr", {

  expect_error(
    create_beta_distr(alpha = "nonsense"),
    "'alpha' must be an alpha parameter"
  )
  expect_error(
    create_beta_distr(beta = "nonsense"),
    "'beta' must be a beta parameter"
  )

  expect_error(
    create_beta_distr(alpha = create_alpha_param(value = -1.0)),
    "'alpha' must have a positive value"
  )

  expect_error(
    create_beta_distr(beta = create_beta_param(value = -1.0)),
    "'beta' must have a value of at least 1.0"
  )

  expect_error(
    create_beta_distr(beta = create_beta_param(value = 0.5)),
    "'beta' must have a value of at least 1.0"
  )

})

test_that("abuse, exp_distr", {

  expect_error(
    create_exp_distr(mean = "nonsense"),
    "'mean' must be a mean parameter"
  )

})

test_that("abuse, gamma_distr", {

  expect_error(
    create_gamma_distr(alpha = "nonsense"),
    "'alpha' must be an alpha parameter"
  )
  expect_error(
    create_gamma_distr(beta = "nonsense"),
    "'beta' must be a beta parameter"
  )

  expect_error(
    create_gamma_distr(alpha = create_alpha_param(value = -1.0)),
    "'value' of 'alpha' must be positive"
  )
  expect_error(
    create_gamma_distr(beta = create_beta_param(value = -1.0)),
    "'value' of 'beta' must be positive"
  )

})

test_that("abuse, inv_gamma_distr", {

  expect_error(
    create_inv_gamma_distr(alpha = "nonsense"),
    "'alpha' must be an alpha parameter"
  )
  expect_error(
    create_inv_gamma_distr(beta = "nonsense"),
    "'beta' must be a beta parameter"
  )

})

test_that("abuse, laplace_distr", {

  expect_error(
    create_laplace_distr(mu = "nonsense"),
    "'mu' must be a mu parameter"
  )
  expect_error(
    create_laplace_distr(scale = "nonsense"),
    "'scale' must be a scale parameter"
  )

})

test_that("abuse, log_normal_distr", {

  expect_error(
    create_log_normal_distr(m = "nonsense"),
    "'m' must be an m parameter"
  )
  expect_error(
    create_log_normal_distr(s = "nonsense"),
    "'s' must be an s parameter"
  )

  expect_error(
    create_log_normal_distr(s = create_s_param(value = -1.0)),
    "'value' of 's' must be positive"
  )

})

test_that("abuse, normal_distr", {

  expect_error(
    create_normal_distr(mean = "nonsense"),
    "'mean' must be a mean.*parameter"
  )
  expect_error(
    create_normal_distr(sigma = "nonsense"),
    "'sigma' must be a sigma parameter"
  )

})

test_that("abuse, poisson", {

  expect_error(
    create_poisson_distr(lambda = "nonsense"),
    "'lambda' must be a lambda parameter"
  )

})

test_that("abuse, uniform", {

  expect_error(
    create_uniform_distr(upper = 0.0),
    "'upper' must be non-zero and positive"
  )

})

test_that("log_normal_distr, complete use", {
  m <- 1.0
  s <- 1.25
  value <- 100.0
  lower <- 0.01
  upper <- 200.0
  log_normal_distr <- create_log_normal_distr(
    m = m,
    s = s,
    value = value,
    lower = lower,
    upper = upper
  )
  expect_equal(m, log_normal_distr$m$value)
  expect_equal(s, log_normal_distr$s$value)
  expect_equal(value, log_normal_distr$value)
  expect_equal(lower, log_normal_distr$lower)
  expect_equal(upper, log_normal_distr$upper)
})
