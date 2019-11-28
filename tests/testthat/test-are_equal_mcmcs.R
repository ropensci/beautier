test_that("identicals", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- mcmc_1
  expect_true(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("chain_length", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- create_mcmc(chain_length = mcmc_1$chain_length + 1000)
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("store_every", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- create_mcmc(chain_length = mcmc_1$store_every + 10000)
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("pre_burnin", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- create_mcmc(pre_burnin = mcmc_1$pre_burnin + 10000)
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("n_init_attempts", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- create_mcmc(n_init_attempts = mcmc_1$n_init_attempts + 1)
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("sample_from_prior", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- create_mcmc(sample_from_prior = !mcmc_1$sample_from_prior)
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("tracelog", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- mcmc_1
  mcmc_2$tracelog <- create_test_tracelog()
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("screenlog", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- mcmc_1
  mcmc_2$screenlog <- create_test_screenlog()
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})

test_that("treelog", {
  mcmc_1 <- create_mcmc()
  mcmc_2 <- mcmc_1
  mcmc_2$treelog <- create_test_treelog()
  expect_false(are_equal_mcmcs(mcmc_1, mcmc_2))
})
