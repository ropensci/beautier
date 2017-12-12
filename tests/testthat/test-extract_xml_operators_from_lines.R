context("extract_xml_operators_from_lines")

test_that("no operators", {

  testthat::expect_equal(
    beautier:::extract_xml_operators_from_lines(c("a", "b")),
    ""
  )

})

test_that("one operator", {

  testthat::expect_equal(
    beautier:::extract_xml_operators_from_lines(
      c(
      "not",
      "<operator id=\"",
      "neither"
      )
    ),
    "<operator id=\""
  )
})

test_that("two operators", {

  testthat::expect_equal(
    beautier:::extract_xml_operators_from_lines(
      c(
      "not",
      "<operator id=\"a\"/>",
      "",
      "<operator id=\"b\"/>",
      "neither too"
      )
    ),
    c(
      "<operator id=\"a\"/>",
      "",
      "<operator id=\"b\"/>"
    )
  )
})

test_that("multiline operators", {

  testthat::expect_equal(
    beautier:::extract_xml_operators_from_lines(
      c(
      "not",
      "<operator id=\"a\"/>",
      "  <something/>",
      "</operator>",
      "neither too"
      )
    ),
    c(
      "<operator id=\"a\"/>",
      "  <something/>",
      "</operator>"
    )
  )
})
