context("create_beast2_input_run")

test_that("usage", {

  testthat::expect_silent(
    create_beast2_input_run(
      ids = "test_output_0",
      mcmc_chainlength = 1000000,
      fixed_crown_age = FALSE,
      initial_phylogeny = NA
    )
  )
})
