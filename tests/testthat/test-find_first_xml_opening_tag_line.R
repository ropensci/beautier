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

  expect_equal(
    find_first_xml_opening_tag_line(lines = lines, section = "a"),
    1
  )

  expect_equal(
    find_first_xml_opening_tag_line(lines = lines, section = "b"),
    4
  )

  expect_equal(
    find_first_xml_opening_tag_line(lines = lines, section = "c"),
    5
  )

  expect_true(
    is_one_na(
      find_first_xml_opening_tag_line(
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

  expect_error(
    find_first_xml_opening_tag_line(lines = lines, section = NA),
    "`section` must be a single string, not `NA`."
  )

  expect_error(
    find_first_xml_opening_tag_line(lines = lines, section = NULL),
    "`section` must be a single string, not `NULL`."
  )

})
