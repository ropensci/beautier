context("find_last_xml_closing_tag_line")

test_that("use", {

  lines <- c(
    "<a>",
    "  text",
    "</a>",
    "<b>",
    "  <c>",
    "    some other text",
    "  </c>",
    "  <c>",
    "    some other text",
    "  </c>",
    "</b>"
  )

  testthat::expect_equal(
    beautier:::find_last_xml_closing_tag_line(lines = lines, section = "a"),
    3
  )

  testthat::expect_equal(
    beautier:::find_last_xml_closing_tag_line(lines = lines, section = "b"),
    11
  )

  testthat::expect_equal(
    beautier:::find_last_xml_closing_tag_line(lines = lines, section = "c"),
    10
  )

  testthat::expect_true(
    is.na(
      beautier:::find_last_xml_closing_tag_line(
        lines = lines,
        section = "absent"
      )
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
    beautier:::find_last_xml_closing_tag_line(lines = lines, section = NA),
    "'section' must be a word"
  )

  testthat::expect_error(
    beautier:::find_last_xml_closing_tag_line(lines = lines, section = NULL),
    "'section' must be a word"
  )

})
