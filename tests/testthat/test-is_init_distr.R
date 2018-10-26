context("is_init_distr")

test_that("basic", {

  expect_false(
    is_init_distr(
      "nonsense"
    )
  )
})

test_that("use beta", {

  expect_false(
    is_init_distr(
      create_beta_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_beta_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_beta_distr(
        id = 1,
        alpha = create_alpha_param(id = 2),
        beta = create_beta_param(id = NA)
      )
    )
  )
  expect_false(
    is_init_distr(
      create_beta_distr(
        id = 1,
        alpha = create_alpha_param(id = NA),
        beta = create_beta_param(id = 3)
      )
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_beta_distr())
    )
  )

})

test_that("use exponential", {

  expect_false(
    is_init_distr(
      create_exp_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_exp_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_exp_distr(
        id = 1,
        mean = create_mean_param(id = NA)
      )
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_exp_distr())
    )
  )

})

test_that("use gamma", {

  expect_false(
    is_init_distr(
      create_gamma_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_gamma_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = NA),
        beta = create_beta_param(id = 3)
      )
    )
  )
  expect_false(
    is_init_distr(
      create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 2),
        beta = create_beta_param(id = NA)
      )
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_gamma_distr())
    )
  )

})

test_that("use inv_gamma", {

  expect_false(
    is_init_distr(
      create_inv_gamma_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_inv_gamma_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_inv_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = NA),
        beta = create_beta_param(id = 3)
      )
    )
  )
  expect_false(
    is_init_distr(
      create_inv_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 2),
        beta = create_beta_param(id = NA)
      )
    )
  )

  expect_true(
    is_init_distr(
      init_distr(create_inv_gamma_distr())
    )
  )

})

test_that("use laplace", {

  expect_false(
    is_init_distr(
      create_laplace_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_laplace_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_laplace_distr(
        id = 1,
        mu = create_mu_param(id = NA),
        scale = create_scale_param(id = 3)
      )
    )
  )
  expect_false(
    is_init_distr(
      create_laplace_distr(
        id = 1,
        mu = create_mu_param(id = 2),
        scale = create_scale_param(id = NA)
      )
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_laplace_distr())
    )
  )

})

test_that("use log_normal", {

  expect_false(
    is_init_distr(
      create_log_normal_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_log_normal_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_log_normal_distr(
        id = 1,
        m = create_m_param(id = NA),
        s = create_s_param(id = 3)
      )
    )
  )
  expect_false(
    is_init_distr(
      create_log_normal_distr(
        id = 1,
        m = create_m_param(id = 2),
        s = create_s_param(id = NA)
      )
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_log_normal_distr())
    )
  )

})

test_that("use normal", {

  expect_false(
    is_init_distr(
      create_normal_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_normal_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_normal_distr(
        id = 1,
        mean = create_mean_param(id = NA),
        sigma = create_sigma_param(id = 3)
      )
    )
  )
  expect_false(
    is_init_distr(
      create_normal_distr(
        id = 1,
        mean = create_mean_param(id = 2),
        sigma = create_sigma_param(id = NA)
      )
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_normal_distr())
    )
  )

})

test_that("use one_div_x", {

  expect_false(
    is_init_distr(
      create_one_div_x_distr()
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_one_div_x_distr())
    )
  )

})

test_that("use poisson", {

  expect_false(
    is_init_distr(
      create_poisson_distr()
    )
  )
  expect_false(
    is_init_distr(
      create_poisson_distr(id = 1)
    )
  )
  expect_false(
    is_init_distr(
      create_poisson_distr(
        id = 1,
        lambda = create_lambda_param(id = NA)
      )
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_poisson_distr())
    )
  )

})

test_that("use uniform", {

  expect_false(
    is_init_distr(
      create_uniform_distr()
    )
  )
  expect_true(
    is_init_distr(
      create_uniform_distr(id = 1)
    )
  )
  expect_true(
    is_init_distr(
      init_distr(create_uniform_distr())
    )
  )

})
