test_that("check_mcmc_nested_sampling, use", {

  expect_silent(
    check_mcmc_nested_sampling(create_ns_mcmc())
  )
  expect_error(check_mcmc_nested_sampling(create_mcmc()))
  expect_error(check_mcmc_nested_sampling("nonsense"))
  expect_error(check_mcmc_nested_sampling(NA))
  expect_error(check_mcmc_nested_sampling(NULL))
  expect_error(check_mcmc_nested_sampling(list(chain_length = 0)))
})

test_that("check_mcmc_nested_sampling: devious", {

  g <- create_ns_mcmc()
  expect_silent(check_mcmc_nested_sampling(g))

  # No 'particle_count'
  h <- g[names(g) != "particle_count"]
  expect_error(check_mcmc_nested_sampling(h))

  # Invalid 'particle_count'
  h <- g
  h$particle_count <- 0
  expect_error(check_mcmc_nested_sampling(h))

  # No 'sub_chain_length'
  h <- g[names(g) != "sub_chain_length"]
  expect_error(check_mcmc_nested_sampling(h))

  # Invalid 'sub_chain_length'
  h <- g
  h$sub_chain_length <- 0
  expect_error(check_mcmc_nested_sampling(h))

  # No 'epsilon'
  h <- g[names(g) != "epsilon"]
  expect_error(check_mcmc_nested_sampling(h))

  # Invalid 'epsilon'
  h <- g
  h$epsilon <- 0.0
  expect_error(check_mcmc_nested_sampling(h))
})
