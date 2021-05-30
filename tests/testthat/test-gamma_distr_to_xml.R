test_that("use, v2.4", {
  expected <- unindent(
    extract_xml_section_from_lines(
      lines = readr::read_lines(get_beautier_path("rln_2_4.xml")),
      section = "distribution"
    )[8:11]
  )
  created <- gamma_distr_to_xml(
    gamma_distr = create_gamma_distr(
      id = "0",
      alpha = create_alpha_param(id = "2", value = "0.5396"),
      beta = create_beta_param(id = "3", value = "0.3819")
    ),
    beauti_options = create_beauti_options_v2_4()
  )
  expect_equal(created, expected)
})

test_that("use, v2.6", {
  expected <- remove_empty_lines(
    unindent(
      extract_xml_section_from_lines(
        lines = readr::read_lines(get_beautier_path("rln_2_6.xml")),
        section = "distribution"
      )[15:21]
    )
  )
  created <- gamma_distr_to_xml(
    gamma_distr = create_gamma_distr(
      id = "0",
      alpha = create_alpha_param(id = "2", value = "0.5396"),
      beta = create_beta_param(id = "3", value = "0.3819")
    ),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_equal(created, expected)
})
