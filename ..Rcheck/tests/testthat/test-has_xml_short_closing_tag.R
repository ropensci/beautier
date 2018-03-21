context("has_xml_short_closing_tag")

test_that("use", {

  testthat::expect_true(
    beautier:::has_xml_short_closing_tag(
      "<text content=\"Hello\"/>"
    )
  )
  testthat::expect_false(
    beautier:::has_xml_short_closing_tag(
      "<text content=\"Hello\"</text>"
    )
  )
  testthat::expect_true(
    beautier:::has_xml_short_closing_tag(
      beautier:::distr_to_xml(create_one_div_x_distr(id = 1))
    )
  )
})
