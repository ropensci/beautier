context("create_beast2_input_run")

test_that("usage", {

  testthat::expect_silent(
    create_beast2_input_run(
      filename_base = "???",
      fasta_filenames = beastscriptr::get_input_fasta_filename(),
      mcmc_chainlength = 1000000,
      tree_priors = create_tree_prior("birth_death"),
      fixed_crown_age = FALSE,
      initial_phylogeny = NA
    )
  )
})
