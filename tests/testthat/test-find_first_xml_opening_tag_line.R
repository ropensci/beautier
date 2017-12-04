context("find_first_xml_opening_tag_line")

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
    beautier:::find_first_xml_opening_tag_line(lines = lines, section = "a"),
    1
  )

  testthat::expect_equal(
    beautier:::find_first_xml_opening_tag_line(lines = lines, section = "b"),
    4
  )

  testthat::expect_equal(
    beautier:::find_first_xml_opening_tag_line(lines = lines, section = "c"),
    5
  )

  testthat::expect_true(
    is.na(
      beautier:::find_first_xml_opening_tag_line(
        lines = lines,
        section = "nonsense"
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
    beautier:::find_first_xml_opening_tag_line(lines = lines, section = NA),
    "'section' must be a word"
  )

  testthat::expect_error(
    beautier:::find_first_xml_opening_tag_line(lines = lines, section = NULL),
    "'section' must be a word"
  )

})
