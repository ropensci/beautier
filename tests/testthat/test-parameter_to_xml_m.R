test_that("deprecated", {
  check_empty_beautier_folder()

  expect_error(parameter_to_xml_m(), "deprecated")

  check_empty_beautier_folder()
})
