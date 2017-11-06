context("create_beast2_input_distribution")

test_that("usa", {
  testthat::expect_silent(
    create_beast2_input_distribution(
      ids = "test_output_0"
    )
  )


  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filenames <- c(fasta_filename_1, fasta_filename_2)

  testthat::expect_silent(
    create_beast2_input_distribution(
      ids = get_ids(fasta_filenames)
    )
  )

})

test_that("abuse", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filenames <- c(fasta_filename_1, fasta_filename_2)

  testthat::expect_error(
    create_beast2_input_distribution(
      ids = get_ids(fasta_filenames),
      site_models = create_jc69_site_models(n = 1) # Should have been 2
    )
  )


})
