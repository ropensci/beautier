test_that("use, JC69, v2.4", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_jc69_site_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_warning(
    site_model_to_xml_lh_distr(
      inference_model = inference_model
    )
  )
})


test_that("deprecation", {
  expect_error(
    site_model_to_xml_lh_distr(
      site_model = "something",
      inference_model = "irrelevant"
    ),
    "'site_model' is deprecated, use 'inference_model' instead"
  )
})
