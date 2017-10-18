context("create_posterior")

test_that("use", {

  posterior <- create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc_chainlength = 10000
  )
  testthat::expect_true(RBeast::is_posterior(posterior))

})
