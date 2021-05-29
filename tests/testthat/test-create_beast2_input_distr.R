test_that("use", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_inference_model(
      beauti_options = create_beauti_options_v2_4()
    )
  )
  xml <- create_beast2_input_distr(
    inference_model = inference_model
  )
  expect_true(is_xml(xml))
})
