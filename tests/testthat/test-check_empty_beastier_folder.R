test_that("tests should have cleaned up all their temp files", {
  expect_silent(check_empty_beautier_folder())
})

test_that("non-existing folder is empty", {
  expect_silent(check_empty_beautier_folder(get_beautier_tempfilename()))
})

test_that("folders are detected", {
  beautier_folder <- get_beautier_tempfilename()
  beautier_subfolder <- file.path(beautier_folder, "subfolder")
  expect_silent(check_empty_beautier_folder(beautier_folder))
  dir.create(beautier_subfolder, showWarnings = FALSE, recursive = TRUE)
  expect_error(check_empty_beautier_folder(beautier_folder))
  unlink(beautier_folder, recursive = TRUE)
  expect_silent(check_empty_beautier_folder(beautier_folder))
})

test_that("files are detected", {
  if (rappdirs::app_dir()$os == "win") return()

  dir.create(get_beautier_folder(), showWarnings = FALSE, recursive = TRUE)
  beautier_filename <- file.path(get_beautier_tempfilename())
  expect_silent(check_empty_beautier_folder(beautier_folder = get_beautier_folder()))
  readr::write_lines(x = "irrelevant", file = beautier_filename)
  file.create(normalizePath(beautier_filename, mustWork = FALSE))
  expect_error(check_empty_beautier_folder(beautier_folder))
  file.remove(beautier_filename)
  expect_silent(check_empty_beautier_folder())
})
