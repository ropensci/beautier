context("has_xml_short_closing_tag")

test_that("use", {

  testthat::expect_true(
    has_xml_short_closing_tag(
      "<text content=\"Hello\"/>" # nolint this is no absolute path
    )
  )
  testthat::expect_false(
    has_xml_short_closing_tag(
      "<text content=\"Hello\"</text>"
    )
  )
  testthat::expect_true(
    has_xml_short_closing_tag(
      distr_to_xml(create_one_div_x_distr(id = 1))
    )
  )
})
