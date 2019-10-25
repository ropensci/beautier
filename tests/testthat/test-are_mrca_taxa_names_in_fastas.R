context("are_mrca_taxa_names_in_fastas")

test_that("use", {

  my_filename <- get_fasta_filename()

  expect_true(
    are_mrca_taxa_names_in_fastas(
      mrca_priors = list(
        create_mrca_prior(
          alignment_id = get_alignment_id(my_filename),
          taxa_names = get_taxa_names(my_filename)
        )
      ),
      fasta_filenames = my_filename
    )
  )

  expect_false(
    are_mrca_taxa_names_in_fastas(
      mrca_priors = list(
        create_mrca_prior(
          alignment_id = get_alignment_id(my_filename),
          taxa_names = paste0("broken_", get_taxa_names(my_filename))
        )
      ),
      fasta_filenames = my_filename
    )
  )

})
