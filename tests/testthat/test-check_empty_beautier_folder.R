test_that("non-existing folder", {
  expect_silent(
    check_empty_beautier_folder(beautier_folder = tempfile())
  )
})

test_that("empty folder", {
  beautier_folder <- tempfile()
  dir.create(beautier_folder)
  expect_silent(
    check_empty_beautier_folder(beautier_folder = beautier_folder)
  )
  unlink(beautier_folder, recursive = TRUE)
})

test_that("folder with subfolder", {
  beautier_folder <- tempfile()
  dir.create(beautier_folder)
  dir.create(file.path(beautier_folder, "subfolder"))

  expect_error(
    check_empty_beautier_folder(beautier_folder = beautier_folder),
    "Folders found in beautier folder"
  )
  unlink(beautier_folder, recursive = TRUE)
})

test_that("folder with file", {
  beautier_folder <- tempfile()
  dir.create(beautier_folder)
  file.create(file.path(beautier_folder, "file.txt"))

  expect_error(
    check_empty_beautier_folder(beautier_folder = beautier_folder),
    "Files found in beautier folder"
  )
  unlink(beautier_folder, recursive = TRUE)
})
