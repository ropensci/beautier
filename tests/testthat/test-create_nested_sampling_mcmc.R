test_that("can create default silently", {
  expect_silent(create_nested_sampling_mcmc())
})

test_that("set and get is symmetric", {
  chain_length <- 1234000
  store_every <- 3000
  particle_count <- 42
  sub_chain_length <- 3000
  epsilon <- 3.14
  mcmc <- create_nested_sampling_mcmc(
    chain_length = chain_length,
    store_every = store_every,
    particle_count = particle_count,
    sub_chain_length = sub_chain_length,
    epsilon = epsilon
  )
  expect_equal(mcmc$chain_length, chain_length)
  expect_equal(mcmc$store_every, store_every)
  expect_equal(mcmc$particle_count, particle_count)
  expect_equal(mcmc$sub_chain_length, sub_chain_length)
  expect_equal(mcmc$epsilon, epsilon)
})

test_that("elements can be retrieved from BEAST2 XML", {
  chain_length <- 1234000
  store_every <- 56000
  particle_count <- 42
  sub_chain_length <- 7000
  epsilon <- 3.14
  nested_sampling_mcmc <- create_nested_sampling_mcmc(
    chain_length = chain_length,
    store_every = store_every,
    particle_count = particle_count,
    sub_chain_length = sub_chain_length,
    epsilon = epsilon
  )

  input_filename <- get_beautier_path("test_output_2.fas")
  text <- create_beast2_input(
    input_filename = input_filename,
    mcmc = nested_sampling_mcmc
  )
  nss_line <- na.omit(stringr::str_match(text, ".*beast.gss.NS.*"))[1,1 ]
  # Line will look something like this:
  #
  # <run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"1234000\" storeEvery=\"56000\" particleCount=\"42\" subChainLength=\"7000\" epsilon=\"3.14\"> # nolint indeed this is a long line
  #
  expect_true(
    !is.na(
      stringr::str_match(
        nss_line, paste0("chainLength=.", chain_length, ". ")
      )
    )
  )
  expect_true(
    !is.na(
      stringr::str_match(
        nss_line, paste0("storeEvery=.", store_every, ". ")
      )
    )
  )
  expect_true(
    !is.na(
      stringr::str_match(
        nss_line, paste0("particleCount=.", particle_count, ". ")
      )
    )
  )
  expect_true(
    !is.na(
      stringr::str_match(
        nss_line, paste0("subChainLength=.", sub_chain_length, ". ")
      )
    )
  )
  expect_true(
    !is.na(
      stringr::str_match(
        nss_line, paste0("epsilon=.", epsilon, ".>")
      )
    )
  )
})
