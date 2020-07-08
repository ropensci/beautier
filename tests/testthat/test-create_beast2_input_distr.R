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

test_that("deprecation", {
  expect_error(
    create_beast2_input_distr(
      site_models = "something",
      inference_model = "irrelevant"
    ),
    "'site_models' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_distr(
      clock_models = "something",
      inference_model = "irrelevant"
    ),
    "'clock_models' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_distr(
      tree_priors = "something",
      inference_model = "irrelevant"
    ),
    "'tree_priors' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_distr(
      mrca_priors = "something",
      inference_model = "irrelevant"
    ),
    "'mrca_priors' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    create_beast2_input_distr(
      tipdates_filename = "something",
      inference_model = "irrelevant"
    ),
    "'tipdates_filename' is deprecated, use 'inference_model' instead"
  )
})
