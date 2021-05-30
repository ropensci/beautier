test_that("Yule, v2.4", {
  expected <- stringr::str_trim(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("2_4.xml")),
      "<operator id="
    )
  )
  inference_model <- create_inference_model()
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- tree_prior_to_xml_operators(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("Yule, v2.6.0", {
  expected <- stringr::str_trim(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("2_6_0.xml")),
      "<operator id="
    )
  )
  inference_model <- create_inference_model(
    beauti_options = create_beauti_options_v2_6()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- tree_prior_to_xml_operators(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
