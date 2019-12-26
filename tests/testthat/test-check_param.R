test_that("use", {

  expect_silent(check_param(create_alpha_param()))
  expect_silent(check_param(create_beta_param()))
  expect_silent(check_param(create_clock_rate_param()))
  expect_silent(check_param(create_m_param()))
  expect_silent(check_param(create_mean_param()))
  expect_silent(check_param(create_mu_param()))
  expect_silent(check_param(create_kappa_1_param()))
  expect_silent(check_param(create_s_param()))
  expect_silent(check_param(create_scale_param()))
  expect_silent(check_param(create_sigma_param()))

  expect_error(check_param("nonsense"))
  expect_error(check_param(NULL))
  expect_error(check_param(NA))
  expect_error(check_param(c()))
  expect_error(check_param(""))
  expect_error(check_param(ape::rcoal(3)))
})

test_that("check_param, devious", {

  g <- create_alpha_param()
  testit::assert(check_param(g))

  # No 'name'
  h <- g[names(g) != "name"]
  expect_error(check_param(h))

  # Invalid 'name'
  h <- g
  h$name <- "nonsense"
  expect_error(check_param(h))

  # No 'id'
  h <- g[names(g) != "id"]
  expect_error(check_param(h))

  # No 'value'
  h <- g[names(g) != "value"]
  expect_error(check_param(h))

  # 'value' is NA
  h <- g
  h$value <- NA
  expect_error(check_param(h))
})
