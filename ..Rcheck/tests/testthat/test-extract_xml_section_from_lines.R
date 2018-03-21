context("extract_xml_section_from_lines")

test_that("use", {

  lines <- c(
    "<a>",
    "  text",
    "</a>",
    "<b>",
    "  <c>",
    "    some other text",
    "  </c>",
    "</b>"
  )

  testthat::expect_equal(
    beautier:::extract_xml_section_from_lines(lines = lines, section = "a"),
    c(
      "<a>",
      "  text",
      "</a>"
    )
  )

  testthat::expect_equal(
    beautier:::extract_xml_section_from_lines(lines = lines, section = "c"),
    c(
      "  <c>",
      "    some other text",
      "  </c>"
    )
  )

})

test_that("use: operators", {

  testthat::expect_silent(
    beautier:::extract_xml_section_from_lines(
      lines = readLines(beautier::get_beautier_path("2_4.xml")),
      section = "operators")
  )

})

test_that("use: loggers", {

  testthat::expect_silent(
    beautier:::extract_xml_section_from_lines(
      lines = readLines(beautier::get_beautier_path("2_4.xml")),
      section = "loggers")
  )

})

test_that("abuse: section must be a word", {

  testthat::expect_error(
    beautier:::extract_xml_section_from_lines(lines = lines, section = NA),
    "'section' must be a word"
  )

  testthat::expect_error(
    beautier:::extract_xml_section_from_lines(lines = lines, section = NULL),
    "'section' must be a word"
  )
})

test_that("abuse: opening tag absent", {

  lines <- c(
    "<a>",
    "  text",
    "</a>"
  )

  testthat::expect_error(
    beautier:::extract_xml_section_from_lines(
      lines = lines, section = "nonsense"
    ),
    "Opening tag for 'section' could not be found in 'lines'"
  )
})

test_that("abuse: closing tag absent", {

  lines <- c(
    "<a>",
    "  text",
    "<no_slash_a>"
  )

  testthat::expect_error(
    beautier:::extract_xml_section_from_lines(
      lines = lines, section = "a"
    ),
    "Closing tag for 'section' could not be found in 'lines'"
  )
})
