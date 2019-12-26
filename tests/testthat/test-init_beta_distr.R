test_that("use", {
  beta_distr <- create_beta_distr()
  expect_true(is.na(beta_distr$alpha$id))
  expect_true(is.na(beta_distr$beta$id))
  beta_distr <- init_beta_distr(
    beta_distr,
    distr_id = 314,
    param_id = 42
  )
  expect_equal(beta_distr$id, 314)
  expect_equal(beta_distr$alpha$id, 42)
  expect_equal(beta_distr$beta$id, 43)
})
