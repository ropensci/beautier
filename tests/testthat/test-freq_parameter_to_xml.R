test_that("minimal use", {

  remove_beautier_folder()
  expect_silent(
    freq_parameter_to_xml(
      freq_parameter = create_freq_param(
        id = "1"
      )
    )
  )
  expect_silent(check_empty_beautier_folder())
})

test_that("use", {

  expected <- paste0(
    "<parameter ",
    "id=\"freqParameter.s:1\" dimension=\"4\" ",
    "lower=\"0.0\" ",
    "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
  )

  created <- freq_parameter_to_xml(
    freq_parameter = create_freq_param(
      id = "1"
    )
  )
  expect_equal(created, expected)
})

test_that("use, v2.4", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("hky_2_4.xml")),
      section = "state"
    )[9]
  )
  created <- freq_parameter_to_xml(
    freq_param = create_freq_param(id = "test_output_0", value = "0.25"),
    beauti_options = create_beauti_options_v2_4()
  )
  expect_equal(created, expected)
  expect_silent(check_empty_beautier_folder())
})

test_that("use, v2.6", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("issue_135_no_mrca_no_estimate_beauti.xml")),
      section = "state"
    )[9]
  )
  created <- freq_parameter_to_xml(
    freq_param = create_freq_param(id = "anthus_aco_sub", value = "0.25"),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_equal(created, expected)
  expect_silent(check_empty_beautier_folder())
})
