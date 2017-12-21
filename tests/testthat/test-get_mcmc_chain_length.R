context("get_mcmc_chain_length")

test_that("use", {

  testthat::expect_equal(
    get_mcmc_chain_length(
      create_mcmc(chain_length = 12345)
    ),
    12345
  )
})

test_that("abuse", {

  testthat::expect_error(
    get_mcmc_chain_length("nonsense")
  )
})
