context("is_mcmc")

test_that("is_mcmc, use", {

  expect_true(is_mcmc(create_mcmc()))
  expect_true(is_mcmc(create_mcmc_nested_sampling()))
  expect_false(is_mcmc("nonsense"))
  expect_false(is_mcmc(NA))
  expect_false(is_mcmc(NULL))
  expect_false(is_mcmc(list(chain_length = 0)))

})

test_that("is_mcmc: devious", {

  g <- create_mcmc()
  expect_true(beautier:::is_mcmc(g))

  # No 'chain_length'
  h <- g[names(g) != "chain_length"]
  expect_false(beautier:::is_mcmc(h))

  # Invalid 'chain_length'
  h <- g
  h$chain_length <- -123
  expect_false(beautier:::is_mcmc(h))

  # No 'store_every'
  h <- g[names(g) != "store_every"]
  expect_false(beautier:::is_mcmc(h))

  # Invalid 'store_every': below -1
  h <- g
  h$store_every <- -123
  expect_false(beautier:::is_mcmc(h))

  # Invalid 'store_every': at zero
  h <- g
  h$store_every <- 0
  expect_false(beautier:::is_mcmc(h))

  # 'store_every' bigger than chain length
  h <- g
  h$store_every <- 100000
  h$chain_length <- 10
  expect_false(beautier:::is_mcmc(h))
})

test_that("is_mcmc_nested_sampling, use", {

  expect_true(
    beautier:::is_mcmc_nested_sampling(create_mcmc_nested_sampling())
  )
  expect_false(beautier:::is_mcmc_nested_sampling(create_mcmc()))
  expect_false(is_mcmc_nested_sampling("nonsense"))
  expect_false(is_mcmc_nested_sampling(NA))
  expect_false(is_mcmc_nested_sampling(NULL))
  expect_false(is_mcmc_nested_sampling(list(chain_length = 0)))
})

test_that("is_mcmc_nested_sampling: devious", {

  g <- create_mcmc_nested_sampling()
  expect_true(beautier:::is_mcmc_nested_sampling(g))

  # No 'particle_count'
  h <- g[names(g) != "particle_count"]
  expect_false(beautier:::is_mcmc_nested_sampling(h))

  # Invalid 'particle_count'
  h <- g
  h$particle_count <- 0
  expect_false(beautier:::is_mcmc_nested_sampling(h))

  # No 'sub_chain_length'
  h <- g[names(g) != "sub_chain_length"]
  expect_false(beautier:::is_mcmc_nested_sampling(h))

  # Invalid 'sub_chain_length'
  h <- g
  h$sub_chain_length <- 0
  expect_false(beautier:::is_mcmc_nested_sampling(h))

  # No 'epsilon'
  h <- g[names(g) != "epsilon"]
  expect_false(beautier:::is_mcmc_nested_sampling(h))

  # Invalid 'epsilon'
  h <- g
  h$epsilon <- 0.0
  expect_false(beautier:::is_mcmc_nested_sampling(h))
})
