test_that("minimal use", {
  expect_silent(clear_beautier_cache())
})

test_that("create file", {
  beautier_cache_folder_name <- dirname(get_beautier_tempfilename())

  filename <- get_beautier_tempfilename()
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  readr::write_lines("irrelevant", file = filename)
  expect_equal(1, length(list.files(beautier_cache_folder_name)))
  expect_silent(clear_beautier_cache())
  expect_equal(0, length(list.files(beautier_cache_folder_name)))
})

test_that("create dir", {
  beautier_cache_folder_name <- dirname(get_beautier_tempfilename())

  folder_name <- get_beautier_tempfilename()
  dir.create(folder_name, showWarnings = FALSE, recursive = TRUE)
  expect_equal(2, length(list.dirs(beautier_cache_folder_name)))
  expect_silent(clear_beautier_cache())
  expect_equal(0, length(list.dirs(beautier_cache_folder_name)))
})
