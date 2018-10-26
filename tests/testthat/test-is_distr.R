context("is_distr")

test_that("use", {

  expect_true(is_distr(create_beta_distr()))
  expect_true(is_distr(create_exp_distr()))
  expect_true(is_distr(create_gamma_distr()))
  expect_true(is_distr(create_inv_gamma_distr()))
  expect_true(is_distr(create_laplace_distr()))
  expect_true(is_distr(create_log_normal_distr()))
  expect_true(is_distr(create_normal_distr()))
  expect_true(is_distr(create_one_div_x_distr()))
  expect_true(is_distr(create_poisson_distr()))
  expect_true(is_distr(create_uniform_distr()))
  expect_false(is_distr(list(name = "nonsense")))

})

test_that("is_beta_distr, devious", {

  g <- create_beta_distr()
  testit::assert(is_beta_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_beta_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_beta_distr(h))

  # No 'alpha'
  h <- g[names(g) != "alpha"]
  expect_false(is_beta_distr(h))

  # Invalid 'alpha'
  h <- g
  h$alpha <- "nonsense"
  expect_false(is_beta_distr(h))

  # No 'beta'
  h <- g[names(g) != "beta"]
  expect_false(is_beta_distr(h))

  # Invalid 'beta'
  h <- g
  h$beta <- "nonsense"
  expect_false(is_beta_distr(h))
})

test_that("is_exp_distr, devious", {

  g <- create_exp_distr()
  testit::assert(is_exp_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_exp_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_exp_distr(h))

  # No 'mean'
  h <- g[names(g) != "mean"]
  expect_false(is_exp_distr(h))

  # Invalid 'mean'
  h <- g
  h$mean <- "mean"
  expect_false(is_exp_distr(h))
})

test_that("is_gamma_distr, devious", {

  g <- create_gamma_distr()
  testit::assert(is_gamma_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_gamma_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_gamma_distr(h))

  # No 'alpha'
  h <- g[names(g) != "alpha"]
  expect_false(is_gamma_distr(h))

  # Invalid 'alpha'
  h <- g
  h$alpha <- "nonsense"
  expect_false(is_gamma_distr(h))

  # No 'beta'
  h <- g[names(g) != "beta"]
  expect_false(is_gamma_distr(h))

  # Invalid 'beta'
  h <- g
  h$beta <- "nonsense"
  expect_false(is_gamma_distr(h))
})

test_that("is_inv_gamma_distr, devious", {

  g <- create_inv_gamma_distr()
  testit::assert(is_inv_gamma_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_inv_gamma_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_inv_gamma_distr(h))

  # No 'alpha'
  h <- g[names(g) != "alpha"]
  expect_false(is_inv_gamma_distr(h))

  # Invalid 'alpha'
  h <- g
  h$alpha <- "nonsense"
  expect_false(is_inv_gamma_distr(h))

  # No 'beta'
  h <- g[names(g) != "beta"]
  expect_false(is_inv_gamma_distr(h))

  # Invalid 'beta'
  h <- g
  h$beta <- "nonsense"
  expect_false(is_inv_gamma_distr(h))
})

test_that("is_laplace_distr, devious", {

  g <- create_laplace_distr()
  testit::assert(is_laplace_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_laplace_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_laplace_distr(h))

  # No 'mu'
  h <- g[names(g) != "mu"]
  expect_false(is_laplace_distr(h))

  # Invalid 'mu'
  h <- g
  h$mu <- "nonsense"
  expect_false(is_laplace_distr(h))

  # No 'scale'
  h <- g[names(g) != "scale"]
  expect_false(is_laplace_distr(h))

  # Invalid 'scale'
  h <- g
  h$scale <- "nonsense"
  expect_false(is_laplace_distr(h))
})

test_that("is_log_normal_distr, devious", {

  g <- create_log_normal_distr()
  testit::assert(is_log_normal_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_log_normal_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_log_normal_distr(h))

  # No 'm'
  h <- g[names(g) != "m"]
  expect_false(is_log_normal_distr(h))

  # Invalid 'm'
  h <- g
  h$m <- "nonsense"
  expect_false(is_log_normal_distr(h))

  # No 's'
  h <- g[names(g) != "s"]
  expect_false(is_log_normal_distr(h))

  # Invalid 's'
  h <- g
  h$s <- "nonsense"
  expect_false(is_log_normal_distr(h))
})

test_that("is_normal_distr, devious", {

  g <- create_normal_distr()
  testit::assert(is_normal_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_normal_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_normal_distr(h))

  # No 'mean'
  h <- g[names(g) != "mean"]
  expect_false(is_normal_distr(h))

  # Invalid 'mean'
  h <- g
  h$mean <- "nonsense"
  expect_false(is_normal_distr(h))

  # No 'sigma'
  h <- g[names(g) != "sigma"]
  expect_false(is_normal_distr(h))

  # Invalid 'sigma'
  h <- g
  h$sigma <- "nonsense"
  expect_false(is_normal_distr(h))
})

test_that("is_one_div_x_distr, devious", {

  g <- create_one_div_x_distr()
  testit::assert(is_one_div_x_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_one_div_x_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_one_div_x_distr(h))
})

test_that("is_poisson_distr, devious", {

  g <- create_poisson_distr()
  testit::assert(is_poisson_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_poisson_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_poisson_distr(h))

  # No 'lambda'
  h <- g[names(g) != "lambda"]
  expect_false(is_poisson_distr(h))

  # Invalid 'lambda'
  h <- g
  h$lambda <- "nonsense"
  expect_false(is_poisson_distr(h))
})

test_that("is_uniform_distr, devious", {

  g <- create_uniform_distr()
  testit::assert(is_uniform_distr(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_uniform_distr(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_uniform_distr(h))
})
