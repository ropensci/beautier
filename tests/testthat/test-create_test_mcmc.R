test_that("use", {
  mcmc <- create_test_mcmc()
  expect_equal(mcmc$chain_length, 3000)
  expect_equal(mcmc$store_every, 1000)
  expect_true(!file.exists(mcmc$tracelog$filename))
  expect_true(nchar(mcmc$screenlog$filename) > 0)
  expect_true(!file.exists(mcmc$treelog$filename))
})
