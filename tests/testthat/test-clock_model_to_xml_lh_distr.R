test_that("warning", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_strict_clock_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_warning(
    clock_model_to_xml_lh_distr(
      inference_model = inference_model
    ),
    "'clock_model_to_xml_lh_distr' is deprecated, use 'create_branch_rate_model_xml' instead" # nolint indeed a long line
  )
})

test_that("deprecation", {

  expect_error(
    clock_model_to_xml_lh_distr(
      clock_model = "something",
      inference_model = "irrelevant"
    ),
    "'clock_model' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    clock_model_to_xml_lh_distr(
      mrca_priors = "something",
      inference_model = "irrelevant"
    ),
    "'mrca_priors' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    clock_model_to_xml_lh_distr(
      tipdates_filename = "something",
      inference_model = "irrelevant"
    ),
    "'tipdates_filename' is deprecated, use 'inference_model' instead"
  )
})
