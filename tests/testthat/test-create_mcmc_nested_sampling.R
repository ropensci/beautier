context("create_ns_mcmc")

test_that("use", {
  expect_true(is_mcmc(create_ns_mcmc()))
  expect_false(is_default_mcmc(create_ns_mcmc()))
  expect_false(is_default_mcmc("nonsense"))
  expect_true(is_mcmc_nested_sampling(create_ns_mcmc()))
})

test_that("abuse", {
  expect_silent(
    create_ns_mcmc()
  )
  expect_error(
    create_ns_mcmc(particle_count = 0),
    "'mcmc.particle_count' must be at least 1"
  )
  expect_error(
    create_ns_mcmc(sub_chain_length = 0),
    "'mcmc.sub_chain_length' must be at least 1"
  )
  expect_error(
    create_ns_mcmc(epsilon = 0.0),
    "'mcmc.epsilon' must be non-zero and positive"
  )
})
