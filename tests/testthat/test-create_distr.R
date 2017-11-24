context("create_distr")

test_that("use", {

  testthat::expect_silent(create_distr(name = "uniform", id = 1))

  testthat::expect_silent(create_uniform_distr(id = 1))

  testthat::expect_silent(create_uniform_distr(id = 1, upper = 1000))
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
