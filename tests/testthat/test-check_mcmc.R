context("test-check_mcmc")

test_that("use", {
  expect_silent(check_mcmc(create_mcmc()))
  expect_silent(check_mcmc(create_mcmc_nested_sampling()))
  expect_silent(check_mcmc(create_nested_sampling_mcmc()))

  # Must stop on non-MCMCs
  expect_error(check_mcmc(mcmc = "nonsense"))
  expect_error(check_mcmc(mcmc = NULL))
  expect_error(check_mcmc(mcmc = NA))
})
