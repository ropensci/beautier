test_that("use", {
  expect_silent(remove_beautier_folder())
  expect_silent(remove_beautier_folder())
  check_empty_beautier_folder()
})
