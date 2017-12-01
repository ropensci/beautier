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
