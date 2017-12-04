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

test_that("abuse", {

  lines <- c(
    "<a>",
    "  text",
    "</a>"
  )

  testthat::expect_error(
    beautier:::extract_xml_section_from_lines(lines = lines, section = "nonsense"),
    "Opening tag for 'section' could not be found in 'lines'"
  )

  testthat::expect_error(
    beautier:::extract_xml_section_from_lines(lines = lines, section = NA),
    "'section' must be a word"
  )

  testthat::expect_error(
    beautier:::extract_xml_section_from_lines(lines = lines, section = NULL),
    "'section' must be a word"
  )

})
