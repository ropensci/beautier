test_that("minimal use", {
  expect_silent(
    alpha_parameter_to_xml(
      alpha_parameter = create_alpha_param(
        id = "1"
      )
    )
  )
  expect_silent(check_empty_beautier_folder())
})

test_that("use, v2.4", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_4.xml")),
      section = "distribution"
    )[9]
  )
  created <- alpha_parameter_to_xml(
    alpha_param = create_alpha_param(id = "2", value = "0.5396"),
    beauti_options = create_beauti_options_v2_4()
  )
  expect_equal(created, expected)
  expect_silent(check_empty_beautier_folder())
})

test_that("use, v2.6", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_6.xml")),
      section = "distribution"
    )[17]
  )
  created <- alpha_parameter_to_xml(
    alpha_param = create_alpha_param(id = "2", value = "0.5396"),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_equal(created, expected)
  expect_silent(check_empty_beautier_folder())
})
