context("parameter_to_xml")

test_that("use", {

  skip("WIP")

  testthat::expect_silent(
    parameter_to_xml(parameter = create_alpha_parameter(id = 1))
  )

  testthat::expect_equal(
    parameter_to_xml(
      parameter = create_uniform_distr(id = 1, upper = "1000.0")),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"1000.0\"/>"
  )

  testthat::expect_equal(
    parameter_to_xml(
      parameter = create_uniform_distr(id = 1, upper = Inf)),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>"
  )

})

test_that("abuse", {

  testthat::expect_error(parameter_to_xml(parameter = "nonsense"))
  testthat::expect_error(parameter_to_xml(parameter = NA))
  testthat::expect_error(parameter_to_xml(parameter = NULL))
  testthat::expect_error(parameter_to_xml(
    parameter = create_uniform_distr()))

})
