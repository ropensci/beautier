test_that("use", {

  fasta_filename <- get_fasta_filename()

  expect_true(
    is_mrca_align_ids_in_fastas(
      mrca_prior = create_mrca_prior(
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)
      ),
      fasta_filenames = fasta_filename
    )
  )

  expect_false(
    is_mrca_align_ids_in_fastas(
      mrca_prior = create_mrca_prior(
        alignment_id = paste0("broken_", get_alignment_id(fasta_filename)),
        taxa_names = get_taxa_names(fasta_filename)
      ),
      fasta_filenames = fasta_filename
    )
  )

})
