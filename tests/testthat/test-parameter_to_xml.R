context("parameter_to_xml")

test_that("use", {

  testthat::expect_silent(
    parameter_to_xml(parameter = create_alpha_parameter(id = 1))
  )

  testthat::expect_equal(
    parameter_to_xml(parameter = create_alpha_parameter(id = 1)),
    "<parameter id=\"RealParameter.1\" estimate=\"false\" name=\"alpha\">0</parameter>" # nolint
  )

})

test_that("abuse", {

  testthat::expect_error(parameter_to_xml(parameter = "nonsense"))
  testthat::expect_error(parameter_to_xml(parameter = NA))
  testthat::expect_error(parameter_to_xml(parameter = NULL))
  testthat::expect_error(parameter_to_xml(parameter = create_uniform_distr()))

})
