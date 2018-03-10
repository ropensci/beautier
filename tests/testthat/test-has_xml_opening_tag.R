context("has_xml_opening_tag")

test_that("use on known tag", {

  text <- c(
    "  <a id=1>",
    "    some text",
    "  </a>"
  )

  testthat::expect_true(
    beautier:::has_xml_opening_tag(text)
  )
  testthat::expect_true(
    beautier:::has_xml_opening_tag(text, section = "a")
  )
  testthat::expect_false(
    beautier:::has_xml_opening_tag(text, section = "nonsense")
  )
  testthat::expect_false(
    beautier:::has_xml_opening_tag("", section = "nonsense")
  )

})



test_that("abuse", {

  text <- c(
    "<a>",
    "  some text",
    "</a>"
  )

  testthat::expect_error(
    beautier:::has_xml_opening_tag(text, section = NULL),
    "'section' must be NA or a word"
  )

})
