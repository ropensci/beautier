test_that("use", {
  expect_silent(
    init_inference_model(
      input_filename = get_fasta_filename(),
      create_inference_model()
    )
  )
})
