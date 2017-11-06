context("create_beast2_input_beast")

test_that("use", {

  testthat::expect_silent(
    create_beast2_input_beast(
      input_fasta_filenames = get_input_fasta_filename())
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
      site_models = create_jc69_site_models(n = 1) # Should be 2
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
