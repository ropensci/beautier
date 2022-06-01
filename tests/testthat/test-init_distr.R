test_that("use", {
  check_empty_beautier_folder()

  uniform_distr <- create_uniform_distr()
  expect_false(is_init_distr(uniform_distr))
  uniform_distr <- init_distr(uniform_distr)
  expect_true(is_init_distr(uniform_distr))
})

test_that("init distr ID", {
  uniform_distr <- create_uniform_distr()
  expect_true(is.na(uniform_distr$id))
  uniform_distr <- init_distr(uniform_distr, distr_id = 314)
  expect_equal(uniform_distr$id, 314)
})

test_that("init beta distr param IDs, both uninit", {
  beta_distr <- create_beta_distr()
  expect_true(is.na(beta_distr$alpha$id))
  expect_true(is.na(beta_distr$beta$id))
  beta_distr <- init_distr(beta_distr, param_id = 42)
  expect_equal(beta_distr$alpha$id, 42)
  expect_equal(beta_distr$beta$id, 43)
})

test_that("init exp distr param IDs, both uninit", {
  exp_distr <- create_exp_distr()
  expect_true(is.na(exp_distr$mean$id))
  exp_distr <- init_distr(exp_distr, param_id = 314)
  expect_equal(exp_distr$mean$id, 314)
})

test_that("init gamma distr param IDs, both uninit", {
  gamma_distr <- create_gamma_distr()
  expect_true(is.na(gamma_distr$alpha$id))
  expect_true(is.na(gamma_distr$beta$id))
  gamma_distr <- init_distr(gamma_distr, param_id = 42)
  expect_equal(gamma_distr$alpha$id, 42)
  expect_equal(gamma_distr$beta$id, 43)
})

test_that("init inv_gamma distr param IDs, both uninit", {
  inv_gamma_distr <- create_inv_gamma_distr()
  expect_true(is.na(inv_gamma_distr$alpha$id))
  expect_true(is.na(inv_gamma_distr$beta$id))
  inv_gamma_distr <- init_distr(inv_gamma_distr, param_id = 42)
  expect_equal(inv_gamma_distr$alpha$id, 42)
  expect_equal(inv_gamma_distr$beta$id, 43)
})

test_that("init laplace distr param IDs, both uninit", {
  laplace_distr <- create_laplace_distr()
  expect_true(is.na(laplace_distr$mu$id))
  expect_true(is.na(laplace_distr$scale$id))
  laplace_distr <- init_distr(laplace_distr, param_id = 42)
  expect_equal(laplace_distr$mu$id, 42)
  expect_equal(laplace_distr$scale$id, 43)
})

test_that("init log_normal distr param IDs, both uninit", {
  log_normal_distr <- create_log_normal_distr()
  expect_true(is.na(log_normal_distr$m$id))
  expect_true(is.na(log_normal_distr$s$id))
  log_normal_distr <- init_distr(log_normal_distr, param_id = 42)
  expect_equal(log_normal_distr$m$id, 42)
  expect_equal(log_normal_distr$s$id, 43)
})

test_that("init normal distr param IDs, both uninit", {
  normal_distr <- create_normal_distr()
  expect_true(is.na(normal_distr$mean$id))
  expect_true(is.na(normal_distr$sigma$id))
  normal_distr <- init_distr(normal_distr, param_id = 42)
  expect_equal(normal_distr$mean$id, 42)
  expect_equal(normal_distr$sigma$id, 43)
})

test_that("init one_div_x", {
  one_div_x_distr <- create_one_div_x_distr()
  expect_true(is.na(one_div_x_distr$id))
  one_div_x_distr <- init_distr(one_div_x_distr, distr_id = 314)
  expect_equal(one_div_x_distr$id, 314)
})

test_that("init poisson distr param IDs, both uninit", {
  poisson_distr <- create_poisson_distr()
  expect_true(is.na(poisson_distr$lambda$id))
  poisson_distr <- init_distr(poisson_distr, param_id = 42)
  expect_equal(poisson_distr$lambda$id, 42)
})

test_that("init uniform", {
  uniform_distr <- create_uniform_distr()
  expect_true(is.na(uniform_distr$id))
  uniform_distr <- init_distr(uniform_distr, distr_id = 314)
  expect_equal(uniform_distr$id, 314)

  check_empty_beautier_folder()
})
