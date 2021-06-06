test_that("v2.4", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model()
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("2_4.xml")),
      section = "state"
    )
  )
  expect_equal(created, expected)
})

test_that("v2.6", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("2_6_2.xml")),
      section = "state"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("v2.6, RLN", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      clock_model = create_rln_clock_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_6.xml")),
      section = "state"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("tipdates, v2.6", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("tipdates_2_6.xml")),
      section = "state"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
