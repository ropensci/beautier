test_that("deprecated", {
  check_empty_beautier_folder()

  expect_error(tree_priors_to_xml_operators(), "deprecated")
})
