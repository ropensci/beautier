context("is_mcmc")

test_that("use", {

  testthat::expect_true(is_mcmc(create_mcmc()))
  testthat::expect_false(is_mcmc("nonsense"))
  testthat::expect_false(is_mcmc(NA))
  testthat::expect_false(is_mcmc(NULL))
  testthat::expect_false(is_mcmc(list(chain_length = 0)))

})
