context("are_equivalent_xml_lines")

test_that("use", {

  lines1 <- readLines(
    beautier::get_beautier_path("gtr_gcc_2_2_4.xml")
  )
  lines2 <- readLines(
    beautier::get_beautier_path("jc69_2_4.xml")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(lines1, lines1)
  )

  testthat::expect_output(
    are_equivalent_xml_lines(lines1, lines2, verbose = TRUE)
  )

  testthat::expect_false(
    are_equivalent_xml_lines(lines1, lines2)
  )

  testthat::expect_output(
    are_equivalent_xml_lines(c("A"), c("B"), verbose = TRUE)
  )


})
