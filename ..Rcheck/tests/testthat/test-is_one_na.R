context("is_one_na")

test_that("use", {

  testthat::expect_true(beautier:::is_one_na(NA))
  testthat::expect_false(beautier:::is_one_na(NULL))
  testthat::expect_false(beautier:::is_one_na(c(NA, NA)))
  testthat::expect_false(beautier:::is_one_na(c(NULL, NULL)))
})
