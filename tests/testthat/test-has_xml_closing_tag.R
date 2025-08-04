test_that("use", {

  text <- c(
    "  <a with some extras>",
    "    some text",
    "  </a>",
    "  <b>",
    "    some other tex",
    "  </b>"
  )

  expect_true(has_xml_closing_tag(text, section = "a"))
  expect_true(has_xml_closing_tag(text, section = "b"))
  expect_false(
    has_xml_closing_tag(text, section = "nonsense")
  )
  expect_false(
    has_xml_closing_tag("", section = "nonsense")
  )

})

test_that("abuse", {

  text <- c(
    "<a>",
    "  some text",
    "</a>",
    "<b>",
    "  some other tex",
    "</b>"
  )

  expect_error(
    has_xml_closing_tag(lines = text, section = NA),
    "must be a single string"
  )
  expect_error(
    has_xml_closing_tag(text, section = NULL),
    "must be a single string"
  )

})
