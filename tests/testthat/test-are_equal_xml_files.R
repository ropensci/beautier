context("are_equal_xml_files")

test_that("use", {

  filename_1 <- filename <- beautier::get_beautier_path("are_equal_xml_files_1.xml")
  filename_2 <- filename <- beautier::get_beautier_path("are_equal_xml_files_1.xml")
  testthat::expect_true(
    are_equal_xml_files(filename_1, filename_2, section = "a")
  )
  testthat::expect_true(
    are_equal_xml_files(filename_1, filename_2, section = "b")
  )
})

test_that("abuse", {

  filename_1 <- filename <- beautier::get_beautier_path("are_equal_xml_files_1.xml")
  filename_2 <- filename <- beautier::get_beautier_path("are_equal_xml_files_2.xml")
  testthat::expect_silent(
    are_equal_xml_files(
      filename_1 = filename_1,
      filename_2 = filename_2,
      section = "a"
    )
  )
  testthat::expect_error(
    are_equal_xml_files(
      filename_1 = "abs.ent",
      filename_2 = filename_2,
      section = "a"
    ),
    "'filename_1' must be the name of a file that is present"
  )
  testthat::expect_error(
    are_equal_xml_files(
      filename_1 = filename_1,
      filename_2 = "abs.ent",
      section = "a"
    ),
    "'filename_2' must be the name of a file that is present"
  )
  testthat::expect_error(
    are_equal_xml_files(
      filename_1 = filename_1,
      filename_2 = filename_2,
      section = "nonsense"
    ),
    "Opening tag for 'section' could not be found in 'lines_1'"
  )

})
