context("indent")

test_that("use", {

  testthat::expect_silent(indent(text = "test", n_spaces = 2))

  text <- c("hello", "world")
  testthat::expect_silent(indent(text = text, n_spaces = 2))

  indented_text_1 <- paste0(" ", c("hello", "world"))
  testthat::expect_equal(indent(text = text, n_spaces = 1), indented_text_1)

  indented_text_2 <- paste0("  ", c("hello", "world"))
  testthat::expect_equal(indent(text = text, n_spaces = 2), indented_text_2)
})

test_that("abuse", {

  testthat::expect_error(indent(text = "test", n_spaces = -1))

})
