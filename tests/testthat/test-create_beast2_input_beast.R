context("create_beast2_input_beast")

test_that("use", {

  fasta_filename <- get_input_fasta_filename()
  testthat::expect_silent(
    create_beast2_input_beast(
      input_fasta_filenames = fasta_filename,
      tree_priors = list(
        create_yule_tree_prior(
          birth_rate_distr = create_uniform_distr(id = 1)
        )
      )
    )
  )
})

test_that("abuse", {

  testthat::expect_error(
    create_beast2_input_beast(input_fasta_filenames = "nonsense")
  )


  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")

  # Two filenames, one site model
  testthat::expect_error(
    create_beast2_input_beast(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      site_models = create_jc69_site_model()
    )
  )

  # Two filenames, one clock model
  testthat::expect_error(
    create_beast2_input_beast(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      clock_models = create_strict_clock_model()
    )
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input_beast(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      tree_priors = create_yule_tree_prior()
    )
  )

  # Two filenames, one phylogeny
  testthat::expect_error(
    create_beast2_input_beast(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      initial_phylogenies = c(ape::rcoal(4))
    )
  )

})
