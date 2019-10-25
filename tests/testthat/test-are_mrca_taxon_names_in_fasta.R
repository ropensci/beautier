test_that("use", {

  fasta_filename <- get_fasta_filename()

  expect_true(
    are_mrca_taxon_names_in_fasta(
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)
      ),
      fasta_filename = fasta_filename
    )
  )

  expect_false(
    are_mrca_taxon_names_in_fasta(
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = paste0("broken_", get_taxa_names(fasta_filename))
      ),
      fasta_filename = fasta_filename
    )
  )

})
