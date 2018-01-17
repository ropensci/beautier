context("mcmc_to_xml_run")

test_that("use, default", {

  created <- beautier:::mcmc_to_xml_run(create_mcmc())
  expected <- c("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"10000000\">")
  testthat::expect_equal(created, expected)

})

test_that("use, default", {

  created <- beautier:::mcmc_to_xml_run(create_mcmc(chain_length = 1000))
  expected <- c("<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1000\">")
  testthat::expect_equal(created, expected)

})
