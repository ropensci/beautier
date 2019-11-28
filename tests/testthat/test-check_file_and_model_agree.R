test_that("cannot create CBS with less than 6 taxa", {

  expect_error(
    check_file_and_model_agree(
      input_filename = get_beautier_path("test_output_2.fas"),
      inference_model = create_inference_model(
        tree_prior = create_cbs_tree_prior()
      )
    ),
    "'group_sizes_dimension' .* must be less than the number of taxa"
  )
})

test_that("MRCA prior's alignment ID must match the FASTA file ID", {
  fasta_filename <- get_fasta_filename()
  inference_model <- create_inference_model(
    mrca_prior = create_mrca_prior(
      alignment_id = paste0("broken_", get_alignment_id(fasta_filename)),
      taxa_names = get_taxa_names(fasta_filename)
    )
  )

  expect_error(
    check_file_and_model_agree(
      input_filename = fasta_filename,
      inference_model = inference_model
    ),
    "All MRCA prior's alignment IDs must match the FASTA file IDs"
  )
})

test_that("MRCA prior's taxa names must be FASTA file taxa names", {

  fasta_filename <- get_fasta_filename()
  inference_model <- create_inference_model(
    mrca_prior = create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = paste0("broken_", get_taxa_names(fasta_filename))
    )
  )
  expect_error(
    check_file_and_model_agree(
      input_filename = fasta_filename,
      inference_model = inference_model
    ),
    "All MRCA prior's taxa names must be FASTA file taxa names"
  )
})
