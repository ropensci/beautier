context("are_equivalent_xml_lines")

test_that("use", {

  lines1 <- readLines(
    system.file("extdata", "gtr_gcc_2_2_4.xml", package = "beautier")
  )
  lines2 <- readLines(
    system.file("extdata", "jc69_2_4.xml", package = "beautier")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(lines1, lines1)
  )

  testthat::expect_false(
    are_equivalent_xml_lines(lines1, lines2)
  )

})
