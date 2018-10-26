context("create_mcmc_nested_sampling")

test_that("use", {
  expect_true(is_mcmc(create_mcmc_nested_sampling()))
  expect_false(is_default_mcmc(create_mcmc_nested_sampling()))
  expect_false(is_default_mcmc("nonsense"))
  expect_true(is_mcmc_nested_sampling(create_mcmc_nested_sampling()))
})
