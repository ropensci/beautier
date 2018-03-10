context("is_xml")

test_that("use", {

  testthat::expect_false(beautier:::is_xml(NA))
  skip("WIP")
  testthat::expect_true(beautier:::is_xml("<text content=\"Hello\"/>"))
  testthat::expect_true(beautier:::is_xml("<text>Hello</text>"))
  testthat::expect_true(beautier:::is_xml(beautier:::parameter_to_xml(create_param_alpha(id = 1))))

  testthat::expect_false(beautier:::is_xml("<text>Hello</invalid_close>"))
})
