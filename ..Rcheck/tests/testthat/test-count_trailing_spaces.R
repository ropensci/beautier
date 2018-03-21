context("count_trailing_spaces")

test_that("use", {


  testthat::expect_equal(beautier:::count_trailing_spaces("x"), 0)
  testthat::expect_equal(beautier:::count_trailing_spaces(" y"), 1)
  testthat::expect_equal(beautier:::count_trailing_spaces("  <"), 2)
  testthat::expect_equal(beautier:::count_trailing_spaces(""), 0)
  testthat::expect_equal(beautier:::count_trailing_spaces(" "), 1)
  testthat::expect_equal(beautier:::count_trailing_spaces("  "), 2)

})
