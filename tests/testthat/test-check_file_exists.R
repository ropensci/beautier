context("test-check_file_exists")

test_that("use", {

  expect_silent(
    check_file_exists(
      get_beautier_path("anthus_aco_sub.fas")
    )
  )

  # Minimal use
  expect_error(
    check_file_exists("absent"),
    "File not found. Could not find file with path 'absent'"
  )

  # Add a description
  absent_filename <- "absent"
  expect_error(
    check_file_exists(absent_filename, "absent_filename"),
    "File 'absent_filename' not found. Could not find file with path 'absent'"
  )
})
