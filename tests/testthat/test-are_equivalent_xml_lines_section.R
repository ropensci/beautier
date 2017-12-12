context("are_equivalent_xml_lines_section")

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
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2, section = "a")
  )
  testthat::expect_false(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2, section = "b")
  )

})

test_that("abuse: section must be a word", {

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2, section = NA),
    "'section' must be a word"
  )

})

test_that("abuse: opening tag of lines 1 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>"
  )

  lines_2 <- lines_1

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "nonsense"),
    "Opening tag for 'section' could not be found in 'lines_1'"
  )
})

test_that("abuse: closing tag of lines 1 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "<oops_a>"
  )

  lines_2 <- lines_1

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "a"),
    "Closing tag for 'section' could not be found in 'lines_1'"
  )
})







test_that("abuse: opening tag of lines 2 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>"
  )

  lines_2 <- c(
    "<b>",
    "  nothing",
    "</b>"
  )

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "a"),
    "Opening tag for 'section' could not be found in 'lines_2'"
  )
})

test_that("abuse: closing tag of lines 2 not found", {

  lines_1 <- c(
    "<a>",
    "  same",
    "</a>"
  )

  lines_2 <- c(
    "<a>",
    "  nothing",
    "<no_slash_a>"
  )

  testthat::expect_error(
    beautier:::are_equivalent_xml_lines_section(lines_1, lines_2,
      section = "a"),
    "Closing tag for 'section' could not be found in 'lines_2'"
  )
})
