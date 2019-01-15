context("test-check_mrca_prior")

test_that("use", {

  fasta_filename <- get_beautier_path("anthus_aco.fas")
  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename = fasta_filename),
    taxa_names = get_taxa_names(filename = fasta_filename)
  )
  mrca_prior <- create_mrca_prior(
    alignment_id = get_alignment_id(fasta_filename = fasta_filename),
    taxa_names = get_taxa_names(filename = fasta_filename)
  )

  expect_silent(check_mrca_prior(mrca_prior))
  # NA is a valid null-MRCA-prior
  expect_silent(check_mrca_prior(mrca_prior = NA))

  expect_error(check_mrca_prior(mrca_prior = "nonsense"))
  expect_error(check_mrca_prior(mrca_prior = NULL))
})
