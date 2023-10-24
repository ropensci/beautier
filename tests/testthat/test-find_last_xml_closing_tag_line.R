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

  expect_equal(
    find_last_xml_closing_tag_line(lines = lines, section = "a"),
    3
  )

  expect_equal(
    find_last_xml_closing_tag_line(lines = lines, section = "b"),
    11
  )

  expect_equal(
    find_last_xml_closing_tag_line(lines = lines, section = "c"),
    10
  )

  expect_true(
    is_one_na(
      find_last_xml_closing_tag_line(
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

  expect_error(
    find_last_xml_closing_tag_line(lines = lines, section = NA),
    "`section` must be a single string, not `NA`."
  )

  expect_error(
    find_last_xml_closing_tag_line(lines = lines, section = NULL),
    "`section` must be a single string, not `NULL`."
  )

})
