context("create_posterior")

test_that("returns a posterior", {

  if (!beautier::is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 2,
    sequence_length = 4,
    mcmc_chainlength = 10000
  )
  testthat::expect_true(RBeast::is_posterior(posterior))

})

test_that("use", {

  if (!beautier::is_on_travis()) return()

  testthat::expect_silent(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc_chainlength = 10000
    )
  )

  testthat::expect_silent(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc_chainlength = 10000,
      fixed_crown_age = TRUE
    )
  )

  testthat::expect_silent(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc_chainlength = 10000,
      fixed_crown_age = TRUE,
      crown_age = 15
    )
  )
})

test_that("abuse", {

  testthat::expect_error(
    create_posterior(
      n_taxa = -1, # Must be positive
      sequence_length = 4,
      mcmc_chainlength = 10000,
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 0, # Must be non-zero positive
      mcmc_chainlength = 10000,
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 1,
      mcmc_chainlength = 42, # Must be at least 10000
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 1,
      mcmc_chainlength = 10000,
      fixed_crown_age = 42, # Must be TRUE or FALSE
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 1,
      mcmc_chainlength = 10000,
      fixed_crown_age = TRUE,
      crown_age = -42 # Must be NA or positive
    )
  )

  # Cannot specify a non-fixed crown age
  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc_chainlength = 10000,
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc_chainlength = 10000,
      tree_priors = "nonsense"
    )
  )

})
