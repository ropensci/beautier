test_that("use", {

  expect_silent(
    parameter_to_xml(
      parameter = create_alpha_param(id = 1),
      beauti_options = create_beauti_options()
    )
  )

  expect_silent(
    parameter_to_xml(
      parameter = create_kappa_param(id = 1),
      beauti_options = create_beauti_options()
    )
  )

  expect_silent(
    parameter_to_xml(
      parameter = create_rate_ct_param(id = 1),
      beauti_options = create_beauti_options()
    )
  )

  expect_equal(
    parameter_to_xml(
      parameter = create_alpha_param(id = 1),
      beauti_options = create_beauti_options()
    ),
    "<parameter id=\"RealParameter.1\" estimate=\"false\" name=\"alpha\">0</parameter>" # nolint
  )
})

test_that("must have IDs", {

  expect_error(
    parameter_to_xml(
      parameter = create_scale_param(id = NA),
      beauti_options = create_beauti_options()
    )
  )

  expect_error(
    parameter_to_xml(
      parameter = create_scale_param(id = 1, value = NA),
      beauti_options = create_beauti_options()
    )
  )

})

test_that("abuse", {


  expect_error(
    parameter_to_xml(
      parameter = "nonsense",
      beauti_options = create_beauti_options()
    )
  )
  expect_error(
    parameter_to_xml(
      parameter = NA,
      beauti_options = create_beauti_options()
    )
  )
  expect_error(
    parameter_to_xml(
      parameter = NULL,
      beauti_options = create_beauti_options()
    )
  )
  expect_error(
    parameter_to_xml(
      parameter = create_uniform_distr(),
      beauti_options = create_beauti_options()
    )
  )

})
