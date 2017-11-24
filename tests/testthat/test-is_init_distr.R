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
        alpha = create_alphaparam(id = 2),
        beta = create_betaparam(id = NA)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_beta_distr(
        id = 1,
        alpha = create_alphaparam(id = NA),
        beta = create_betaparam(id = 3)
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
        mean = create_meanparam(id = NA)
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
        alpha = create_alphaparam(id = NA),
        beta = create_betaparam(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_gamma_distr(
        id = 1,
        alpha = create_alphaparam(id = 2),
        beta = create_betaparam(id = NA)
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
        alpha = create_alphaparam(id = NA),
        beta = create_betaparam(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_inv_gamma_distr(
        id = 1,
        alpha = create_alphaparam(id = 2),
        beta = create_betaparam(id = NA)
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
        mu = create_muparam(id = NA),
        scale = create_scaleparam(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_laplace_distr(
        id = 1,
        mu = create_muparam(id = 2),
        scale = create_scaleparam(id = NA)
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
        m = create_mparam(id = NA),
        s = create_sparam(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_log_normal_distr(
        id = 1,
        m = create_mparam(id = 2),
        s = create_sparam(id = NA)
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
        mean = create_meanparam(id = NA),
        sigma = create_sigmaparam(id = 3)
      )
    )
  )
  testthat::expect_false(
    beautier:::is_init_distr(
      create_normal_distr(
        id = 1,
        mean = create_meanparam(id = 2),
        sigma = create_sigmaparam(id = NA)
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
        lambda = create_lambdaparam(id = NA)
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
