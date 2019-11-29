context("count_trailing_spaces")

test_that("use", {

  expect_equal(count_trailing_spaces("x"), 0)
  expect_equal(count_trailing_spaces(" y"), 1)
  expect_equal(count_trailing_spaces("  <"), 2)
  expect_equal(count_trailing_spaces(""), 0)
  expect_equal(count_trailing_spaces(" "), 1)
  expect_equal(count_trailing_spaces("  "), 2)

})
