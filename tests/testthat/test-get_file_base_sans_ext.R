context("get_file_base_sans_ext")

test_that("use", {

  # Single
  testthat::expect_equal(
    get_file_base_sans_ext("/home/richel/test.txt"), # nolint this is no absolute path
    "test"
  )

  # Single
  testthat::expect_equal(
    get_file_base_sans_ext(
      c("/home/richel/test1.txt", "/home/richel/test2.txt") # nolint this is no absolute path
    ),
    c("test1", "test2")
  )

})
