test_that("cannot create CBS with less than 6 taxa", {

  expect_error(
    check_fasta_file_and_inference_model(
      input_filename = get_beautier_path("test_output_2.fas"),
      inference_model = create_inference_model(
        tree_prior = create_cbs_tree_prior()
      )
    ),
    "'group_sizes_dimension' .* must be less than the number of taxa"
  )
})
