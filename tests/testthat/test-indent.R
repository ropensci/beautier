test_that("use, single line", {

  input <- "test"
  created <- indent(text = input, n_spaces = 2)
  expected <- "  test"
  testthat::expect_equal(created, expected)

})

test_that("use, two lines", {

  input <- c("hello", "world")
  created <- indent(text = input, n_spaces = 2)
  expected <- c("  hello", "  world")
  testthat::expect_equal(created, expected)

})

test_that("indent two lines with zero spaces", {

  input <- c("hello", "world")
  created <- indent(text = input, n_spaces = 0)
  expected <- input
  testthat::expect_equal(created, expected)

})

test_that("do not indent whitespace", {

  input <- ""
  created <- indent(text = input, n_spaces = 2)
  expected <- ""
  testthat::expect_equal(created, expected)

})

test_that("abuse", {

  testthat::expect_error(
    indent(text = "test", n_spaces = -1),
    "`n_spaces` must be a whole number"
  )

})
