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

  # Tested in-depth by 'check_mcmc'

  testthat::expect_error(
    create_mcmc(chain_length = -1234),
    "chain_length"
  )

  testthat::expect_error(
    create_mcmc(
      chain_length = 10,
      store_every = -2
    ),
    "store_every"
  )
  testthat::expect_error(
    create_mcmc(
      chain_length = 10000,
      store_every = 11000
    ),
    "store_every.*chain_length"
  )

})
