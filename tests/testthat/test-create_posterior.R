context("create_posterior")

test_that("returns a posterior", {

  if (!is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 2,
    sequence_length = 4,
    mcmc = create_mcmc(chain_length = 10000)
  )
  testthat::expect_true(beastier::is_posterior(posterior))

})

test_that("use", {

  if (!is_on_travis()) return()

  testthat::expect_silent(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc = create_mcmc(chain_length = 10000)
    )
  )

  testthat::expect_silent(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc = create_mcmc(chain_length = 10000),
      fixed_crown_age = TRUE
    )
  )

  testthat::expect_silent(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc = create_mcmc(chain_length = 10000),
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
      mcmc = create_mcmc(chain_length = 10000),
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 0, # Must be non-zero positive
      mcmc = create_mcmc(chain_length = 10000),
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 1,
      mcmc = "nonsense",
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 1,
      mcmc = create_mcmc(chain_length = 10000),
      fixed_crown_age = 42, # Must be TRUE or FALSE
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 1,
      mcmc = create_mcmc(chain_length = 10000),
      fixed_crown_age = TRUE,
      crown_age = -42 # Must be NA or positive
    )
  )

  # Cannot specify a non-fixed crown age
  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc = create_mcmc(chain_length = 10000),
      fixed_crown_age = FALSE,
      crown_age = 15
    )
  )

  testthat::expect_error(
    create_posterior(
      n_taxa = 2,
      sequence_length = 4,
      mcmc = create_mcmc(chain_length = 10000),
      tree_priors = "nonsense"
    )
  )

})

test_that("A fixed crown age must have equal TreeHeights", {

  if (!is_on_travis()) return()

  posterior <- create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc = create_mcmc(chain_length = 10000),
    tree_priors = create_bd_tree_prior(),
    fixed_crown_age = TRUE
  )
  testthat::expect_true(all(posterior$estimates$TreeHeight
    == posterior$estimates$TreeHeight[1]))
})


test_that(paste0("Fixed and specified crown age must result in a posterior ",
  "with that TreeHeight"), {

  if (!is_on_travis()) return()

  crown_age <- 123
  posterior <- beautier:::create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc = create_mcmc(chain_length = 10000),
    fixed_crown_age = TRUE,
    crown_age = crown_age
  )
  testthat::expect_equal(posterior$estimates$TreeHeight[1], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(posterior$estimates$TreeHeight[10], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(crown_age,
    beautier:::get_phylo_crown_age(posterior$trees$STATE_10000),
    tolerance = 0.001)
})

test_that("Two fixed crown ages, interface", {

  if (!is_on_travis()) return()

  crown_age <- 123
  posterior <- beautier:::create_posterior(
    n_taxa = 5,
    sequence_length = 10,
    mcmc = create_mcmc(chain_length = 10000),
    fixed_crown_age = TRUE,
    crown_age = crown_age
  )
  testthat::expect_equal(posterior$estimates$TreeHeight[1], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(posterior$estimates$TreeHeight[10], crown_age,
    tolerance = 0.001)
  testthat::expect_equal(crown_age,
    beautier:::get_phylo_crown_age(posterior$trees$STATE_10000),
    tolerance = 0.001)
})
