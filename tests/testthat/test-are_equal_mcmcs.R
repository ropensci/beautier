context("test-are_equal_mcmcs")

test_that("use", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- create_mcmc()
  mcmc_3 <- create_mcmc(chain_length = mcmc_1$chain_length + 1000)
  mcmc_4 <- create_mcmc(chain_length = mcmc_1$store_every + 10000)
  expect_true(are_equal_mcmcs(mcmc_1, mcmc_2))
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_3))
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_4))
})
