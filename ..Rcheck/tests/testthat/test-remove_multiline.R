context("remove_multiline")

test_that("one of three", {

  text <- c("a", "b", "c")
  testthat::expect_equal(
    beautier:::remove_multiline(text, lines_to_remove = c("a")),
    c("b", "c")
  )

  testthat::expect_equal(
    beautier:::remove_multiline(text, lines_to_remove = c("b")),
    c("a", "c")
  )

  testthat::expect_equal(
    beautier:::remove_multiline(text, lines_to_remove = c("c")),
    c("a", "b")
  )
})

test_that("three of five", {

  text <- c("a", "b", "c", "d", "e")
  testthat::expect_equal(
    beautier:::remove_multiline(text, lines_to_remove = c("a", "b", "c")),
    c("d", "e")
  )
  testthat::expect_equal(
    beautier:::remove_multiline(text, lines_to_remove = c("b", "c", "d")),
    c("a", "e")
  )
  testthat::expect_equal(
    beautier:::remove_multiline(text, lines_to_remove = c("c", "d", "e")),
    c("a", "b")
  )
})
