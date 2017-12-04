context("are_equal_xml_lines")

test_that("use", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>",
    "<b>",
    "  different from lines_2",
    "</b>"
  )

  lines_2 <- c(
    "<a>",
    "  same",
    "</a>",
    "<b>",
    "  different from lines_1",
    "</b>"
  )

  testthat::expect_true(
    are_equal_xml_lines(lines_1, lines_2, section = "a")
  )
  testthat::expect_false(
    are_equal_xml_lines(lines_1, lines_2, section = "b")
  )

})

test_that("abuse", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>",
    "<b>",
    "  different from lines_2",
    "</b>"
  )

  lines_2 <- c(
    "<a>",
    "  same",
    "</a>",
    "<b>",
    "  different from lines_1",
    "</b>"
  )

  testthat::expect_error(
    are_equal_xml_lines(lines_1, lines_2, section = "nonsense"),
    "Opening tag for 'section' could not be found in 'lines_1'"
  )
  testthat::expect_error(
    are_equal_xml_lines(lines_1, lines_2, section = NA),
    "'section' must be a word"
  )

})
