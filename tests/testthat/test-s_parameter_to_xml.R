test_that("minimal use", {
  expect_silent(
    s_parameter_to_xml(
      create_s_param(id = 1),
      beauti_options = create_beauti_options()
    )
  )
})

test_that("Numeric value 'Inf' becomes text 'Infinity'", {
  s_param <- create_s_param(id = 1)
  s_param$upper <- Inf
  text <- s_parameter_to_xml(
    s_param,
    beauti_options = create_beauti_options()
  )
  expect_match(text, "Infinity")
})

test_that("reproduce same as in BEAUti v2.4 file", {
  xml <- readr::read_lines(beautier::get_beautier_path("tn93_2_4.xml"))
  expected <- unindent(stringr::str_subset(xml, "RealParameter.4.*name=.S."))
  created <- s_parameter_to_xml(
    create_s_param(id = 4, value = 1.25, upper = NA),
    beauti_options = create_beauti_options_v2_4()
  )
  expect_equal(created, expected)
})

test_that("reproduce same as in BEAUti v2.6 file", {
  xml <- readr::read_lines(
    beautier::get_beautier_path("issue_135_no_mrca_no_estimate_beauti.xml")
  )
  expected <- unindent(stringr::str_subset(xml, "name=.S."))
  created <- s_parameter_to_xml(
    create_s_param(id = 2, value = 1.25, upper = NA),
    beauti_options = create_beauti_options_v2_6()
  )
  expect_equal(created, expected)
})
