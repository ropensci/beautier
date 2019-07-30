context("are_equivalent_xml_files")

test_that("use", {

  filename1 <- beautier::get_beautier_path("gtr_gcc_2_2_4.xml")
  filename2 <- beautier::get_beautier_path("jc69_2_4.xml")

  testthat::expect_true(
    are_equivalent_xml_files(filename1, filename1)
  )

  testthat::expect_false(
    are_equivalent_xml_files(filename1, filename2)
  )

})

test_that("abuse", {

  filename <- get_beautier_path("gtr_gcc_2_2_4.xml")

  expect_error(
    are_equivalent_xml_files("nonse.nse", filename),
    "File 'filename_1' not found. Could not find file with path 'nonse.nse'"
  )

  expect_error(
    are_equivalent_xml_files(filename, "nonse.nse"),
    "File 'filename_2' not found. Could not find file with path 'nonse.nse'"
  )

})
