context("compare_lines")

test_that("compare all", {

  testthat::expect_silent(
    beautier:::compare_lines(
      lines = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      expected = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      section = NA
    )
  )
})

test_that("compare state", {

  testthat::expect_silent(
    beautier:::compare_lines(
      lines = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      expected = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      section = "state"
    )
  )
})

test_that("compare operators", {

  testthat::expect_silent(
    beautier:::compare_lines(
      lines = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      expected = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      section = "operators"
    )
  )
})
