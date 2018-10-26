context("interspace")

test_that("use, zero lines", {

  input <- NULL
  result <- interspace(input)
  expected <- NULL
  expect_equal(result, expected)

})

test_that("use, one line", {

  input <- c("<a/>")
  result <- interspace(input)
  expected <- input
  expect_equal(result, expected)

})

test_that("use, two lines", {

  input <- c(
    "<a/>",
    "<b/>"
    )
  result <- interspace(input)
  expected <- c(
    "<a/>",
    "",
    "<b/>"
    )
  expect_equal(result, expected)

})

test_that("use, simple indentation", {

  input <- c(
    "<a>",
    "  indented",
    "</a>"
    )
  result <- interspace(input)
  expected <- input
  expect_equal(result, expected)

})

test_that("use, complex indentation", {

  input <- c(
    "<a>",
    "  indented",
    "</a>",
    "<b>",
    "<c>"
    )
  result <- interspace(input)
  expected <- c(
    "<a>",
    "  indented",
    "</a>",
    "",
    "<b>",
    "",
    "<c>"
    )
  expect_equal(result, expected)

})
