context("has_xml_closing_tag")

test_that("use", {

  text <- c(
    "  <a with some extras>",
    "    some text",
    "  </a>",
    "  <b>",
    "    some other tex",
    "  </b>"
  )

  testthat::expect_true(beautier:::has_xml_closing_tag(text, section = "a"))
  testthat::expect_true(beautier:::has_xml_closing_tag(text, section = "b"))
  testthat::expect_false(
    beautier:::has_xml_closing_tag(text, section = "nonsense"))
  testthat::expect_false(
    beautier:::has_xml_closing_tag("", section = "nonsense"))

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

  testthat::expect_error(
    beautier:::has_xml_closing_tag(text, section = NA),
    "'section' must be a word"
  )
  testthat::expect_error(
    beautier:::has_xml_closing_tag(text, section = NULL),
    "'section' must be a word"
  )

})
