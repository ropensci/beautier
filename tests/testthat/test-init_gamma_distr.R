test_that("use", {
  gamma_distr <- create_gamma_distr()
  expect_true(is.na(gamma_distr$alpha$id))
  expect_true(is.na(gamma_distr$beta$id))
  gamma_distr <- init_gamma_distr(
    gamma_distr,
    distr_id = 314,
    param_id = 42
  )
  expect_equal(gamma_distr$id, 314)
  expect_equal(gamma_distr$alpha$id, 42)
  expect_equal(gamma_distr$beta$id, 43)
})
