test_that("strict", {
  inference_model <- create_inference_model(
    clock_model = create_strict_clock_model()
  )
  expect_true(
    is.null(
      clock_model_to_xml_prior_distr(
        inference_model = inference_model
      )
    )
  )
})

test_that("rln, v2.4", {
  # Tested in detail in test-rln_clock_model_to_xml_prior_distr
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(),
    beauti_options = create_beauti_options_v2_4()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expect_silent(
    clock_model_to_xml_prior_distr(
      inference_model = inference_model
    )
  )
})

test_that("rln, v2.6", {
  # Tested in detail in test-rln_clock_model_to_xml_prior_distr
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(),
    beauti_options = create_beauti_options_v2_6()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expect_silent(
    clock_model_to_xml_prior_distr(
      inference_model = inference_model
    )
  )
})

test_that("rln + mrca with distr", {
  # Tested in detail in test-rln_clock_model_to_xml_prior_distr
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(),
    mrca_prior = create_mrca_prior(),
    beauti_options = create_beauti_options_v2_4()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expect_silent(
    clock_model_to_xml_prior_distr(
      inference_model = inference_model
    )
  )
})
