context("are_mrca_taxa_names_in_fastas")

test_that("use", {

  fasta_filename <- get_fasta_filename()

  testthat::expect_true(
    beautier:::are_mrca_taxa_names_in_fastas(
      mrca_taxa_names = get_taxa_names(fasta_filename),
      fasta_filenames = fasta_filename
    )
  )

})
