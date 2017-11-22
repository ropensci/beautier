context("is_init_distr")

test_that("use beta", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_beta_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_beta_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_beta_distr(
        id = 1,
        alpha = create_alpha_parameter(id = 2),
        beta = create_beta_parameter(id = NA)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_beta_distr(
        id = 1,
        alpha = create_alpha_parameter(id = NA),
        beta = create_beta_parameter(id = 3)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_beta_distr())
    )
  )

})

test_that("use exponential", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_exponential_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_exponential_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_exponential_distr(
        id = 1,
        mean = create_mean_parameter(id = NA)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_exponential_distr())
    )
  )

})

test_that("use gamma", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_gamma_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_gamma_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_gamma_distr(
        id = 1,
        alpha = create_alpha_parameter(id = NA),
        beta = create_beta_parameter(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_gamma_distr(
        id = 1,
        alpha = create_alpha_parameter(id = 2),
        beta = create_beta_parameter(id = NA)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_gamma_distr())
    )
  )

})

test_that("use inv_gamma", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_inv_gamma_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_inv_gamma_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_inv_gamma_distr(
        id = 1,
        alpha = create_alpha_parameter(id = NA),
        beta = create_beta_parameter(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_inv_gamma_distr(
        id = 1,
        alpha = create_alpha_parameter(id = 2),
        beta = create_beta_parameter(id = NA)
      )
    )
  )

  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_inv_gamma_distr())
    )
  )

})

test_that("use laplace", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_laplace_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_laplace_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_laplace_distr(
        id = 1,
        mu = create_mu_parameter(id = NA),
        scale = create_scale_parameter(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_laplace_distr(
        id = 1,
        mu = create_mu_parameter(id = 2),
        scale = create_scale_parameter(id = NA)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_laplace_distr())
    )
  )

})

test_that("use log_normal", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_log_normal_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_log_normal_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_log_normal_distr(
        id = 1,
        m = create_m_parameter(id = NA),
        s = create_s_parameter(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_log_normal_distr(
        id = 1,
        m = create_m_parameter(id = 2),
        s = create_s_parameter(id = NA)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_log_normal_distr())
    )
  )

})

test_that("use normal", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_normal_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_normal_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_normal_distr(
        id = 1,
        mean = create_mean_parameter(id = NA),
        sigma = create_sigma_parameter(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_normal_distr(
        id = 1,
        mean = create_mean_parameter(id = 2),
        sigma = create_sigma_parameter(id = NA)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_normal_distr())
    )
  )

})

test_that("use one_div_x", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_one_div_x_distr()
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_one_div_x_distr())
    )
  )

})

test_that("use poisson", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_poisson_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_poisson_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_poisson_distr(
        id = 1,
        lambda = create_lambda_parameter(id = NA)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_poisson_distr())
    )
  )

})

test_that("use uniform", {

  testthat::expect_false(
    beautier:::is_init_distr(
      create_uniform_distr()
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      create_uniform_distr(id = 1)
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_uniform_distr())
    )
  )

})
