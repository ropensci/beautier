context("create_mcmc_nested_sampling")

test_that("use", {
  expect_true(is_mcmc(create_mcmc_nested_sampling()))
  expect_false(is_default_mcmc(create_mcmc_nested_sampling()))
  expect_false(is_default_mcmc("nonsense"))
  expect_true(is_mcmc_nested_sampling(create_mcmc_nested_sampling()))
})

test_that("abuse", {
  expect_silent(
    create_mcmc_nested_sampling()
  )
  expect_error(
    create_mcmc_nested_sampling(particle_count = 0),
    "'particle_count' must be a non-zero amount"
  )
  expect_error(
    create_mcmc_nested_sampling(sub_chain_length = 0),
    "'sub_chain_length' must be a non-zero amount"
  )
  expect_error(
    create_mcmc_nested_sampling(epsilon = 0.0),
    "'epsilon' must be a non-zero number"
  )
})
