context("create_mcmc_nested_sampling")

test_that("use", {
  expect_true(beautier:::is_mcmc(create_mcmc_nested_sampling()))
  expect_false(beautier:::is_default_mcmc(create_mcmc_nested_sampling()))
  expect_true(beautier:::is_mcmc_nested_sampling(create_mcmc_nested_sampling()))
})
