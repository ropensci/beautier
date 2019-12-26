context("parameter_to_xml")

test_that("use", {

  expect_silent(
    parameter_to_xml(parameter = create_alpha_param(id = 1))
  )

  expect_silent(
    parameter_to_xml(parameter = create_rate_ct_param(id = 1))
  )

  expect_equal(
    parameter_to_xml(parameter = create_alpha_param(id = 1)),
    "<parameter id=\"RealParameter.1\" estimate=\"false\" name=\"alpha\">0</parameter>" # nolint
  )

  # Exposing bug of https://github.com/ropensci/babette/issues/26
  expect_equal(
    parameter_to_xml(parameter = create_s_param(id = 1)),
    "<parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0\" name=\"S\" upper=\"Infinity\">0</parameter>" # nolint
  )
})

test_that("must have IDs", {

  expect_error(
    parameter_to_xml(parameter = create_scale_param(id = NA))
  )

  expect_error(
    parameter_to_xml(parameter = create_scale_param(id = 1, value = NA))
  )

})

test_that("abuse", {


  expect_error(parameter_to_xml(parameter = "nonsense"))
  expect_error(parameter_to_xml(parameter = NA))
  expect_error(parameter_to_xml(parameter = NULL))
  expect_error(parameter_to_xml(parameter = create_uniform_distr()))

})
