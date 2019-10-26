test_that("use", {
  mcmc <- create_test_ns_mcmc()
  expect_equal(mcmc$chain_length, 2000)
  expect_equal(mcmc$particle_count, 1)
})
