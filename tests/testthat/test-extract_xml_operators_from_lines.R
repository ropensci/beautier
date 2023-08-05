context("extract_xml_operators_from_lines")

test_that("no operators", {

  testthat::expect_equal(
    extract_xml_operators_from_lines(c("a", "b")),
    ""
  )

})

test_that("one operator", {

  testthat::expect_equal(
    extract_xml_operators_from_lines(
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
    extract_xml_operators_from_lines(
      c(
        "not",
        "<operator id=\"a\"/>", # nolint this is no absolute path
        "",
        "<operator id=\"b\"/>", # nolint this is no absolute path
        "neither too"
      )
    ),
    c(
      "<operator id=\"a\"/>", # nolint this is no absolute path
      "",
      "<operator id=\"b\"/>" # nolint this is no absolute path
    )
  )
})

test_that("multiline operators", {

  testthat::expect_equal(
    extract_xml_operators_from_lines(
      c(
        "not",
        "<operator id=\"a\"/>", # nolint this is no absolute path
        "  <something/>",
        "</operator>",
        "neither too"
      )
    ),
    c(
      "<operator id=\"a\"/>", # nolint this is no absolute path
      "  <something/>",
      "</operator>"
    )
  )
})
