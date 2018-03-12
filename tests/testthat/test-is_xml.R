context("is_xml")

test_that("use", {

  testthat::expect_false(beautier:::is_xml(NA))
  testthat::expect_true(beautier:::is_xml("<text content=\"Hello\"/>"))
  testthat::expect_true(beautier:::is_xml(
    beautier:::parameter_to_xml(create_param_alpha(id = 1)))
  )
  testthat::expect_true(beautier:::is_xml(
    beautier:::distr_to_xml(create_one_div_x_distr(id = 1)))
  )
  testthat::expect_false(beautier:::is_xml("<text id=1></invalid_close>"))
  testthat::expect_false(beautier:::is_xml("<text content=\"Hello\">"))
})
