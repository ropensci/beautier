context("create_mcmc")

test_that("use", {

  testthat::expect_silent(
    create_mcmc()
  )

  testthat::expect_silent(
    create_mcmc(chain_length = 10000)
  )

  testthat::expect_silent(
    create_mcmc(
      chain_length = 10000,
      store_every = 1000
    )
  )
  testthat::expect_silent(
    create_mcmc(
      chain_length = 10000,
      store_every = -1
    )
  )
  testthat::expect_silent(
    create_mcmc(
      chain_length = 10000,
      store_every = NA
    )
  )

})

test_that("abuse", {

  testthat::expect_error(
    create_mcmc(chain_length = -1234),
    "'chain_length' must be positive and non-zero"
  )

  testthat::expect_error(
    create_mcmc(
      chain_length = 10,
      store_every = -2
    ),
    "'store_every' must be at least 1000, NA or -1"
  )
  testthat::expect_error(
    create_mcmc(
      chain_length = 10000,
      store_every = 11000
    ),
    "'store_every' must be equal or lower to 'chain_length'"
  )

})
