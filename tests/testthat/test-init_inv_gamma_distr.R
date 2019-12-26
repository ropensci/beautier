test_that("use", {
  inv_gamma_distr <- create_inv_gamma_distr()
  expect_true(is.na(inv_gamma_distr$alpha$id))
  expect_true(is.na(inv_gamma_distr$beta$id))
  inv_gamma_distr <- init_inv_gamma_distr(
    inv_gamma_distr,
    distr_id = 314,
    param_id = 42
  )
  expect_equal(inv_gamma_distr$id, 314)
  expect_equal(inv_gamma_distr$alpha$id, 42)
  expect_equal(inv_gamma_distr$beta$id, 43)
})
