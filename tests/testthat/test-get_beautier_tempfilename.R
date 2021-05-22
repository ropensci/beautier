test_that("use", {
  expect_silent(get_beautier_tempfilename())
  expect_silent(get_beautier_tempfilename(pattern = "test"))
  expect_silent(get_beautier_tempfilename(fileext = ".xml"))
})
