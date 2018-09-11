context("is_mcmc")

test_that("is_mcmc, use", {

  testthat::expect_true(is_mcmc(create_mcmc()))
  testthat::expect_true(is_mcmc(create_mcmc_nested_sampling()))
  testthat::expect_false(is_mcmc("nonsense"))
  testthat::expect_false(is_mcmc(NA))
  testthat::expect_false(is_mcmc(NULL))
  testthat::expect_false(is_mcmc(list(chain_length = 0)))

})

test_that("is_mcmc_nested_sampling, use", {

  testthat::expect_true(beautier:::is_mcmc_nested_sampling(create_mcmc_nested_sampling()))
  testthat::expect_false(beautier:::is_mcmc_nested_sampling(create_mcmc()))
  testthat::expect_false(is_mcmc_nested_sampling("nonsense"))
  testthat::expect_false(is_mcmc_nested_sampling(NA))
  testthat::expect_false(is_mcmc_nested_sampling(NULL))
  testthat::expect_false(is_mcmc_nested_sampling(list(chain_length = 0)))
})
