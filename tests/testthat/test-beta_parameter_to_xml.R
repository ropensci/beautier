test_that("use, v2.4", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_4.xml")),
      section = "distribution"
    )[10]
  )
  created <- beta_parameter_to_xml(
    beta_parameter = create_beta_param(id = "3", value = "0.3819"),
    beauti_options = create_beauti_options_v2_4()
  )
  expect_equal(created, expected)
})

test_that("use, v2.6", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_6.xml")),
      section = "distribution"
    )[19]
  )
  created <- beta_parameter_to_xml(
    beta_parameter = create_beta_param(id = "3", value = "0.3819"),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_equal(created, expected)
})
