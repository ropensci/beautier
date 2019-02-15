context("mcmc_to_xml_run")

test_that("use, default", {

  created <- mcmc_to_xml_run(create_mcmc())
  expected <- "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1e+07\">"
  expect_equal(created, expected)

})

test_that("use, 1K", {

  created <- mcmc_to_xml_run(create_mcmc(chain_length = 1000))
  expected <- "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1000\">"
  expect_equal(created, expected)

})

test_that("use, store every", {

  created <- mcmc_to_xml_run(
    create_mcmc(chain_length = 2000, store_every = 1000)
  )
  expected <- "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"2000\" storeEvery=\"1000\">" # nolint XML can be long
  expect_equal(created, expected)
})

test_that("use, mcmc_to_xml_run, 1K", {

  created <- mcmc_to_xml_run(create_mcmc_nested_sampling(chain_length = 1000))
  expected <- "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"1000\" particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">" # nolint indeed a long string
  expect_equal(created, expected)

})

test_that("use, mcmc_to_xml_run, store every", {

  created <- mcmc_to_xml_run(create_mcmc_nested_sampling(
    chain_length = 10000, store_every = 1000)
  )
  expected <- "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"10000\" storeEvery=\"1000\" particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">" # nolint indeed a long string
  expect_equal(created, expected)

})
