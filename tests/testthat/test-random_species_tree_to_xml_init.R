context("random_species_tree_to_xml_init")

test_that("use", {

  testthat::expect_silent(
    random_species_tree_to_xml_init("my_id")
  )

})

test_that("abuse", {

  testthat::expect_error(
    random_species_tree_to_xml_init(ape::roal(3))
  )

})
