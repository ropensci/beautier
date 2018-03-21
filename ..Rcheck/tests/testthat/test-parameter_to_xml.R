context("parameter_to_xml")

test_that("use", {

  testthat::expect_silent(
    parameter_to_xml(parameter = create_alpha_param(id = 1))
  )

  testthat::expect_silent(
    parameter_to_xml(parameter = create_rate_ct_param(id = 1))
  )

  testthat::expect_equal(
    parameter_to_xml(parameter = create_alpha_param(id = 1)),
    "<parameter id=\"RealParameter.1\" estimate=\"false\" name=\"alpha\">0</parameter>" # nolint
  )

})

test_that("must have IDs", {

  testthat::expect_error(
    parameter_to_xml(parameter = create_scale_param(id = NA)),
    "parameter must have an ID"
  )

  testthat::expect_error(
    parameter_to_xml(parameter = create_scale_param(id = 1, value = NA)),
    "parameter must have a value"
  )

})

test_that("abuse", {


  testthat::expect_error(parameter_to_xml(parameter = "nonsense"))
  testthat::expect_error(parameter_to_xml(parameter = NA))
  testthat::expect_error(parameter_to_xml(parameter = NULL))
  testthat::expect_error(parameter_to_xml(parameter = create_uniform_distr()))

})
