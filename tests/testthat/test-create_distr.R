context("create_distr")

test_that("use, default", {

  # English function names
  testthat::expect_silent(create_beta_distr())
  testthat::expect_silent(create_exp_distr())
  testthat::expect_silent(create_gamma_distr())
  testthat::expect_silent(create_inv_gamma_distr())
  testthat::expect_silent(create_laplace_distr())
  testthat::expect_silent(create_log_normal_distr())
  testthat::expect_silent(create_normal_distr())
  testthat::expect_silent(create_one_div_x_distr())
  testthat::expect_silent(create_poisson_distr())
  testthat::expect_silent(create_uniform_distr())

  # Search-tree friendly function names
  testthat::expect_silent(create_distr_beta())
  testthat::expect_silent(create_distr_exp())
  testthat::expect_silent(create_distr_gamma())
  testthat::expect_silent(create_distr_inv_gamma())
  testthat::expect_silent(create_distr_laplace())
  testthat::expect_silent(create_distr_log_normal())
  testthat::expect_silent(create_distr_normal())
  testthat::expect_silent(create_distr_one_div_x())
  testthat::expect_silent(create_distr_poisson())
  testthat::expect_silent(create_distr_uniform())

})

test_that("use, parameters", {

  # English function names
  testthat::expect_silent(
    create_beta_distr(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  testthat::expect_silent(
    create_exp_distr(id = 1, mean = create_mean_param())
  )
  testthat::expect_silent(
    create_gamma_distr(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  testthat::expect_silent(
    create_inv_gamma_distr(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  testthat::expect_silent(
    create_laplace_distr(
      id = 1, mu = create_mu_param(), scale = create_scale_param()
    )
  )
  testthat::expect_silent(
    create_log_normal_distr(id = 1, m = create_m_param(), s = create_s_param())
  )
  testthat::expect_silent(
    create_normal_distr(
      id = 1, mean = create_mean_param(), sigma = create_sigma_param()
    )
  )
  testthat::expect_silent(
    create_one_div_x_distr(id = 1)
  )
  testthat::expect_silent(
    create_poisson_distr(id = 1, lambda = create_lambda_param())
  )
  testthat::expect_silent(
    create_uniform_distr()
  )

  # Search-tree friendly function names
  testthat::expect_silent(
    create_distr_beta(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  testthat::expect_silent(
    create_distr_exp(
      id = 1, mean = create_mean_param()
    )
  )
  testthat::expect_silent(
    create_distr_gamma(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  testthat::expect_silent(
    create_distr_inv_gamma(
      id = 1, alpha = create_alpha_param(), beta = create_beta_param()
    )
  )
  testthat::expect_silent(
    create_distr_laplace(
      id = 1, mu = create_mu_param(), scale = create_scale_param()
    )
  )
  testthat::expect_silent(
    create_distr_log_normal(
      id = 1, m = create_m_param(), s = create_s_param()
    )
  )
  testthat::expect_silent(
    create_distr_normal(
      id = 1, mean = create_mean_param(), sigma = create_sigma_param()
    )
  )
  testthat::expect_silent(
    create_distr_one_div_x(id = 1)
  )
  testthat::expect_silent(
    create_distr_poisson(id = 1, lambda = create_lambda_param())
  )
  testthat::expect_silent(create_distr_uniform())

})



test_that("abuse", {

  testthat::expect_error(
    create_distr(name = "nonsense"),
    "'name' must be a distribution name"
  )

})

test_that("abuse, beta_distr", {

  testthat::expect_error(
    create_beta_distr(alpha = "nonsense"),
    "'alpha' must be an alpha parameter"
  )
  testthat::expect_error(
    create_beta_distr(beta = "nonsense"),
    "'beta' must be a beta parameter"
  )

  testthat::expect_error(
    create_beta_distr(alpha = create_alpha_param(value = -1.0)),
    "'alpha' must have a positive value"
  )

  testthat::expect_error(
    create_beta_distr(beta = create_beta_param(value = -1.0)),
    "'beta' must have a value of at least 1.0"
  )

  testthat::expect_error(
    create_beta_distr(beta = create_beta_param(value = 0.5)),
    "'beta' must have a value of at least 1.0"
  )

})

test_that("abuse, exp_distr", {

  testthat::expect_error(
    create_exp_distr(mean = "nonsense"),
    "'mean' must be a mean parameter"
  )

})

test_that("abuse, gamma_distr", {

  testthat::expect_error(
    create_gamma_distr(alpha = "nonsense"),
    "'alpha' must be an alpha parameter"
  )
  testthat::expect_error(
    create_gamma_distr(beta = "nonsense"),
    "'beta' must be a beta parameter"
  )

  testthat::expect_error(
    create_gamma_distr(alpha = create_alpha_param(value = -1.0)),
    "'value' of 'alpha' must be positive"
  )
  testthat::expect_error(
    create_gamma_distr(beta = create_beta_param(value = -1.0)),
    "'value' of 'beta' must be positive"
  )

})

test_that("abuse, inv_gamma_distr", {

  testthat::expect_error(
    create_inv_gamma_distr(alpha = "nonsense"),
    "'alpha' must be an alpha parameter"
  )
  testthat::expect_error(
    create_inv_gamma_distr(beta = "nonsense"),
    "'beta' must be a beta parameter"
  )

})

test_that("abuse, laplace_distr", {

  testthat::expect_error(
    create_laplace_distr(mu = "nonsense"),
    "'mu' must be a mu parameter"
  )
  testthat::expect_error(
    create_laplace_distr(scale = "nonsense"),
    "'scale' must be a scale parameter"
  )

})

test_that("abuse, log_normal_distr", {

  testthat::expect_error(
    create_log_normal_distr(m = "nonsense"),
    "'m' must be an m parameter"
  )
  testthat::expect_error(
    create_log_normal_distr(s = "nonsense"),
    "'s' must be an s parameter"
  )

  testthat::expect_error(
    create_log_normal_distr(s = create_s_param(value = -1.0)),
    "'value' of 's' must be positive"
  )

})

test_that("abuse, normal_distr", {

  testthat::expect_error(
    create_normal_distr(mean = "nonsense"),
    "'mean' must be a mean parameter"
  )
  testthat::expect_error(
    create_normal_distr(sigma = "nonsense"),
    "'sigma' must be a sigma parameter"
  )

})

test_that("abuse, poisson", {

  testthat::expect_error(
    create_poisson_distr(lambda = "nonsense"),
    "'lambda' must be a lambda parameter"
  )

})

test_that("abuse, uniform", {

  testthat::expect_error(
    create_uniform_distr(upper = 0.0),
    "'upper' must be non-zero and positive"
  )

})
