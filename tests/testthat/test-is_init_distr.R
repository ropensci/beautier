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
        alpha = create_alpha_param(id = 2),
        beta = create_beta_param(id = NA)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_beta_distr(
        id = 1,
        alpha = create_alpha_param(id = NA),
        beta = create_beta_param(id = 3)
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
      create_exp_distr()
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_exp_distr(id = 1)
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_exp_distr(
        id = 1,
        mean = create_mean_param(id = NA)
      )
    )
  )
  testthat::expect_true(
    beautier:::is_init_distr(
      beautier:::init_distr(create_exp_distr())
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
        alpha = create_alpha_param(id = NA),
        beta = create_beta_param(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 2),
        beta = create_beta_param(id = NA)
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
        alpha = create_alpha_param(id = NA),
        beta = create_beta_param(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_inv_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 2),
        beta = create_beta_param(id = NA)
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
        mu = create_mu_param(id = NA),
        scale = create_scale_param(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_laplace_distr(
        id = 1,
        mu = create_mu_param(id = 2),
        scale = create_scale_param(id = NA)
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
        m = create_m_param(id = NA),
        s = create_s_param(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_log_normal_distr(
        id = 1,
        m = create_m_param(id = 2),
        s = create_s_param(id = NA)
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
        mean = create_mean_param(id = NA),
        sigma = create_sigma_param(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_normal_distr(
        id = 1,
        mean = create_mean_param(id = 2),
        sigma = create_sigma_param(id = NA)
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
        lambda = create_lambda_param(id = NA)
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
