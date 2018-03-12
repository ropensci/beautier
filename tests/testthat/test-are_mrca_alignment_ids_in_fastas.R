context("are_mrca_align_ids_in_fastas")

test_that("use", {

  fasta_filename <- get_fasta_filename()

  testthat::expect_true(
    beautier:::are_mrca_align_ids_in_fastas(
      mrca_priors = list(
          create_mrca_prior(
          alignment_id = get_alignment_id(fasta_filename),
          taxa_names = get_taxa_names(fasta_filename)
        )
      ),
      fasta_filenames = fasta_filename
    )
  )

  testthat::expect_false(
    beautier:::are_mrca_align_ids_in_fastas(
      mrca_priors = list(
        create_mrca_prior(
          alignment_id = paste0("broken_", get_alignment_id(fasta_filename)),
          taxa_names = get_taxa_names(fasta_filename)
        )
      ),
      fasta_filenames = fasta_filename
    )
  )

})
