test_that("minimal use", {
  remove_beautier_folder()
  expect_silent(
    kappa_param_to_xml(
      kappa_param = create_kappa_param(
        id = "1"
      )
    )
  )
  expect_silent(check_empty_beautier_folder())
})

test_that("use, v2.4", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("hky_2_4.xml")),
      section = "state"
    )[8]
  )
  created <- kappa_param_to_xml(
    kappa_param = create_kappa_param(id = "test_output_0", value = "2.0"),
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
    )[8]
  )
  created <- kappa_param_to_xml(
    kappa_param = create_kappa_param(id = "anthus_aco_sub", value = "2.0"),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_equal(created, expected)
  expect_silent(check_empty_beautier_folder())
})
