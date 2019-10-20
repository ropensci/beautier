context("create_mcmc")

test_that("use", {

  expect_silent(
    create_mcmc()
  )

  expect_silent(
    create_mcmc(chain_length = 10000)
  )

  expect_silent(
    create_mcmc(
      chain_length = 10000,
      store_every = 1000
    )
  )
  expect_silent(
    create_mcmc(
      chain_length = 10000,
      store_every = -1
    )
  )
  expect_silent(
    create_mcmc(
      chain_length = 10000,
      store_every = NA
    )
  )
  expect_silent(
    create_mcmc(
      chain_length = 1e7,
      store_every = 1e3,
      pre_burnin = 1e6
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
