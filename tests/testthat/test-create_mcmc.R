context("create_mcmc")

test_that("use", {

  testthat::expect_silent(
    create_mcmc()
  )

  testthat::expect_silent(
    create_mcmc(chain_length = 10000)
  )

})

test_that("abuse", {

  testthat::expect_error(
    create_mcmc(chain_length = -1234)
  )

})
