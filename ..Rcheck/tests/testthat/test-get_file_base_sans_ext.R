context("get_file_base_sans_ext")

test_that("use", {

  # Single
  testthat::expect_equal(
    get_file_base_sans_ext("/home/richel/test.txt"),
    "test"
  )

  # Single
  testthat::expect_equal(
    get_file_base_sans_ext(
      c("/home/richel/test1.txt", "/home/richel/test2.txt")
    ),
    c("test1", "test2")
  )

})
