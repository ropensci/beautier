context("interspace")

test_that("use, one line", {

  lines <- "a"
  result <- beautier:::interspace(lines)
  testthat::expect_equal(length(result), 2)

})

test_that("use, two lines", {

  lines <- c("a", "b")
  result <- beautier:::interspace(lines)
  testthat::expect_equal(length(result), 4)

})

test_that("use, two lines", {

  skip("WIP")

  input <- c(
    "<a>",
    "  indented",
    "</a>"
    )
  result <- beautier:::interspace(input)
  expected <- input
  testthat::expect_equal(result, expected)

})
