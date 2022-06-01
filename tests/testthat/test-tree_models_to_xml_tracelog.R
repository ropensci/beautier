test_that("deprecated", {
  check_empty_beautier_folder()

  expect_error(tree_models_to_xml_tracelog(), "deprecated")

  check_empty_beautier_folder()
})
