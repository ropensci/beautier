context("are_equivalent_xml_files")

test_that("use", {

  filename1 <- system.file("extdata", "gtr_gcc_2_2_4.xml", package = "beautier")
  filename2 <- system.file("extdata", "jc69_2_4.xml", package = "beautier")

  testthat::expect_true(
    beautier:::are_equivalent_xml_files(filename1, filename1)
  )

  testthat::expect_false(
    beautier:::are_equivalent_xml_files(filename1, filename2)
  )

})

test_that("abuse", {

  filename <- system.file("extdata", "gtr_gcc_2_2_4.xml", package = "beautier")

  testthat::expect_error(
    beautier:::are_equivalent_xml_files("nonse.nse", filename),
    "'filename_1' must be the name of a present file"
  )

  testthat::expect_error(
    beautier:::are_equivalent_xml_files(filename, "nonse.nse"),
    "'filename_2' must be the name of a present file"
  )

})

test_that("modified files must be equivalent", {

  filename_1 <- system.file("extdata",
    "aco_nd2_nd3_nd4_complex_2_4.xml", package = "beautier")
  filename_2 <- system.file("extdata",
    "aco_nd2_nd3_nd4_complex_modified_2_4.xml", package = "beautier")

  testthat::expect_true(
    beautier:::are_equivalent_xml_files(filename_1, filename_2)
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_files(filename_1, filename_2,
      section = "state")
  )

})
