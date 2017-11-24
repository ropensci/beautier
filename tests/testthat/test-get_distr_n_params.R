context("get_distr_n_params")

test_that("use", {

  # alpha and beta
  testthat::expect_equal(get_distr_n_params(create_beta_distr()), 2)

  # mean
  testthat::expect_equal(get_distr_n_params(create_exp_distr()), 1)

  # alpha and beta
  testthat::expect_equal(get_distr_n_params(create_gamma_distr()), 2)

  # alpha and beta
  testthat::expect_equal(get_distr_n_params(create_inv_gamma_distr()), 2)

  # mu and scale
  testthat::expect_equal(get_distr_n_params(create_laplace_distr()), 2)

  # m and s
  testthat::expect_equal(get_distr_n_params(create_log_normal_distr()), 2)

  # mean and sigma
  testthat::expect_equal(get_distr_n_params(create_normal_distr()), 2)

  # none
  testthat::expect_equal(get_distr_n_params(create_one_div_x_distr()), 0)

  # lambda
  testthat::expect_equal(get_distr_n_params(create_poisson_distr()), 1)

  # none
  testthat::expect_equal(get_distr_n_params(create_uniform_distr()), 0)

})

test_that("use", {

  testthat::expect_error(
    get_distr_n_params(distr = "nonsense"),
    "'distr' must be a distribution"
  )
})
