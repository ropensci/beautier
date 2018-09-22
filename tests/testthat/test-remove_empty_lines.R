context("remove_empty_lines")

test_that("use", {

  expect_equal(
    beautier:::remove_empty_lines(lines = c("A", "B")),
    c("A", "B")
  )
  expect_equal(
    beautier:::remove_empty_lines(lines = c("A", "", "B")),
    c("A", "B")
  )
  expect_equal(
    beautier:::remove_empty_lines(lines = c("A", " ", "B")),
    c("A", "B")
  )
  expect_equal(
    beautier:::remove_empty_lines(lines = c("A", "\t", "B")),
    c("A", "B")
  )
  expect_equal(
    beautier:::remove_empty_lines(lines = c("  A", "B  ")),
    c("  A", "B  ")
  )

  expect_equal(
    beautier:::remove_empty_lines(lines = c("  A", "B  "), trim = TRUE),
    c("A", "B")
  )
})
