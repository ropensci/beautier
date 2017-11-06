context("get_mcmc_chain_length")

test_that("use", {

  testthat::expect_equal(
    get_mcmc_chain_length(create_mcmc()),
    get_default_mcmc_chain_length()
  )
})
