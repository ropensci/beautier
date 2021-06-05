test_that("use, v2.4, RLN", {
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(),
    beauti_options = create_beauti_options_v2_4()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- yule_tree_prior_to_xml_operators(
    inference_model = inference_model
  )
  expected <- "<operator id=\"YuleBirthRateScaler.t:test_output_0\" spec=\"ScaleOperator\" parameter=\"@birthRate.t:test_output_0\" scaleFactor=\"0.75\" weight=\"3.0\"/>" # nolint indeed long
  expect_equal(created, expected)
})

test_that("use, v2.6, RLN", {
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(),
    beauti_options = create_beauti_options_v2_6()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- yule_tree_prior_to_xml_operators(
    inference_model = inference_model
  )
  expected <- "<operator id=\"YuleBirthRateScaler.t:test_output_0\" spec=\"ScaleOperator\" parameter=\"@birthRate.t:test_output_0\" weight=\"3.0\"/>" # nolint indeed long
  expect_equal(created, expected)
})
