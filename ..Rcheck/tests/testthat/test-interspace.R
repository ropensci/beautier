context("interspace")

test_that("use, one line", {

  input <- c("<a/>")
  result <- beautier:::interspace(input)
  expected <- input
  testthat::expect_equal(result, expected)

})

test_that("use, two lines", {

  input <- c(
    "<a/>",
    "<b/>"
    )
  result <- beautier:::interspace(input)
  expected <- c(
    "<a/>",
    "",
    "<b/>"
    )
  testthat::expect_equal(result, expected)

})

test_that("use, simple indentation", {

  input <- c(
    "<a>",
    "  indented",
    "</a>"
    )
  result <- beautier:::interspace(input)
  expected <- input
  testthat::expect_equal(result, expected)

})

test_that("use, complex indentation", {

  input <- c(
    "<a>",
    "  indented",
    "</a>",
    "<b>",
    "<c>"
    )
  result <- beautier:::interspace(input)
  expected <- c(
    "<a>",
    "  indented",
    "</a>",
    "",
    "<b>",
    "",
    "<c>"
    )
  testthat::expect_equal(result, expected)

})
