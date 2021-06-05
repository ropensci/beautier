test_that("minimal use, v2.4", {

  inference_model <- create_inference_model()
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expect_silent(
    create_beast2_input_operators(
      inference_model = inference_model
    )
  )
})

test_that("minimal use, v2.6", {

  inference_model <- create_inference_model(
    beauti_options = create_beauti_options_v2_6()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expect_silent(
    create_beast2_input_operators(
      inference_model = inference_model
    )
  )
})

test_that("detailed use, v2.4", {

  expected <- unindent(
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
  created <- create_beast2_input_operators(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("detailed use, v2.6", {
  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("2_6_2.xml")),
      "<operator id="
    )
  )
  inference_model <- create_inference_model()
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- create_beast2_input_operators(
    inference_model = inference_model
  )
  compare_lines(
    lines = created,
    expected = expected,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("detailed use, v2.4, RLN", {
  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("rln_2_4.xml")),
      "<operator id="
    )
  )
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(),
    beauti_options = create_beauti_options_v2_4()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- create_beast2_input_operators(
    inference_model = inference_model
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("detailed use, v2.6, RLN", {
  skip("WIP HIERO")
  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("rln_2_6.xml")),
      "<operator id="
    )
  )
  inference_model <- create_inference_model(
    clock_model = create_rln_clock_model(),
    beauti_options = create_beauti_options_v2_6()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  created <- create_beast2_input_operators(
    inference_model = inference_model
  )
  compare_lines(
    lines = created,
    expected = expected,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
