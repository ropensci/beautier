test_that("compare all", {

  created_lines_filename <- get_beautier_tempfilename()
  expected_lines_filename  <- get_beautier_tempfilename()

  expect_silent(
    compare_lines(
      lines = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      expected = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      section = NA,
      created_lines_filename = created_lines_filename,
      expected_lines_filename = expected_lines_filename
    )
  )

  file.remove(created_lines_filename)
  file.remove(expected_lines_filename)

  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("compare state", {

  created_lines_filename <- get_beautier_tempfilename()
  expected_lines_filename  <- get_beautier_tempfilename()

  expect_silent(
    compare_lines(
      lines = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      expected = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      section = "state",
      created_lines_filename = created_lines_filename,
      expected_lines_filename = expected_lines_filename
    )
  )

  file.remove(created_lines_filename)
  file.remove(expected_lines_filename)

  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("compare operators", {

  created_lines_filename <- get_beautier_tempfilename()
  expected_lines_filename  <- get_beautier_tempfilename()

  expect_silent(
    compare_lines(
      lines = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      expected = readLines(beautier::get_beautier_path("bd_2_4.xml")),
      section = "operators",
      created_lines_filename = created_lines_filename,
      expected_lines_filename = expected_lines_filename
    )
  )

  file.remove(created_lines_filename)
  file.remove(expected_lines_filename)

  remove_beautier_folder()
  check_empty_beautier_folder()
})
