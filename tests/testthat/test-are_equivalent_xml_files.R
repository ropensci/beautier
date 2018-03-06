context("are_equivalent_xml_files")

test_that("use", {

  filename1 <- beautier::get_beautier_path("gtr_gcc_2_2_4.xml")
  filename2 <- beautier::get_beautier_path("jc69_2_4.xml")

  testthat::expect_true(
    beautier:::are_equivalent_xml_files(filename1, filename1)
  )

  testthat::expect_false(
    beautier:::are_equivalent_xml_files(filename1, filename2)
  )

})

test_that("abuse", {

  filename <- beautier::get_beautier_path("gtr_gcc_2_2_4.xml")

  testthat::expect_error(
    beautier:::are_equivalent_xml_files("nonse.nse", filename),
    "'filename_1' must be the name of a present file"
  )

  testthat::expect_error(
    beautier:::are_equivalent_xml_files(filename, "nonse.nse"),
    "'filename_2' must be the name of a present file"
  )

})
