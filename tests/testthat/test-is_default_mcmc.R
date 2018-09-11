context("is_default_mcmc")

test_that("use", {
  expect_true(is_default_mcmc(create_mcmc()))
})
