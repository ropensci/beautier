test_that("use", {

  expect_true(
    has_xml_short_closing_tag(
      "<text content=\"Hello\"/>" # nolint this is no absolute path
    )
  )
  expect_false(
    has_xml_short_closing_tag(
      "<text content=\"Hello\"</text>"
    )
  )
  expect_true(
    has_xml_short_closing_tag(
      distr_to_xml(
        create_one_div_x_distr(id = 1),
        beauti_options = create_beauti_options()
      )
    )
  )
})
