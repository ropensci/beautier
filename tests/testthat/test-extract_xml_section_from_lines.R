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

  expect_equal(
    extract_xml_section_from_lines(lines = lines, section = "a"),
    c(
      "<a>",
      "  text",
      "</a>"
    )
  )

  expect_equal(
    extract_xml_section_from_lines(lines = lines, section = "c"),
    c(
      "  <c>",
      "    some other text",
      "  </c>"
    )
  )

})

test_that("use: operators", {

  expect_silent(
    extract_xml_section_from_lines(
      lines = readLines(get_beautier_path("2_4.xml")),
      section = "operators"
    )
  )

})

test_that("use: loggers", {

  expect_silent(
    extract_xml_section_from_lines(
      lines = readLines(get_beautier_path("2_4.xml")),
      section = "loggers"
    )
  )

})

test_that("abuse: section must be one string", {

  expect_error(
    extract_xml_section_from_lines(lines = lines, section = NA),
    "beautier::is_one_string"
  )

  expect_error(
    extract_xml_section_from_lines(lines = lines, section = NULL),
    "beautier::is_one_string"
  )
})

test_that("abuse: opening tag absent", {

  lines <- c(
    "<a>",
    "  text",
    "</a>"
  )

  expect_error(
    extract_xml_section_from_lines(
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

  expect_error(
    extract_xml_section_from_lines(
      lines = lines, section = "a"
    ),
    "Closing tag for 'section' could not be found in 'lines'"
  )
})
