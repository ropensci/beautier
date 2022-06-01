test_that("use", {
  remove_beautier_folder()

  folder_name <- dirname(get_beautier_tempfilename())
  unlink(folder_name, recursive = TRUE)
  expect_false(dir.exists(folder_name))
  create_beautier_tempfolder()
  expect_true(dir.exists(folder_name))
  unlink(folder_name, recursive = TRUE)

  check_empty_beautier_folder()
})
