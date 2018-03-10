context("is_xml")

test_that("use", {

  testthat::expect_false(beautier:::is_xml(NA))
  testthat::expect_true(beautier:::is_xml(beautier:::parameter_to_xml(create_param_alpha(id = 1))))

})
