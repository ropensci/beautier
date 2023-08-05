test_that("add upper", {

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = NA),
      beauti_options = create_beauti_options()
    ),
    "<Uniform id=\"Uniform.1\" name=\"distr\"/>" # nolint this is no absolute path
  )

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = "1000.0"),
      beauti_options = create_beauti_options()
    ),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"1000.0\"/>" # nolint this is no absolute path
  )

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = Inf),
      beauti_options = create_beauti_options()
    ),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>" # nolint this is no absolute path
  )

})

test_that("abuse", {
  beauti_options <- create_beauti_options()
  expect_error(distr_to_xml(distr = "nonsense", beauti_options))
  expect_error(distr_to_xml(distr = NA, beauti_options))
  expect_error(distr_to_xml(distr = NULL, beauti_options))

  expect_error(
    distr_to_xml(distr = create_uniform_distr(id = NA), beauti_options),
    "distribution must have an ID"
  )

  expect_error(
    distr_to_xml(distr = create_laplace_distr(id = NA), beauti_options),
    "distribution must have an ID"
  )

  expect_error(
    distr_to_xml(distr = create_laplace_distr(id = 1), beauti_options)
  )
})

test_that("add upper", {

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = NA),
      beauti_options = create_beauti_options()
    ),
    "<Uniform id=\"Uniform.1\" name=\"distr\"/>" # nolint this is no absolute path
  )

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = "1000.0"),
      beauti_options = create_beauti_options()
    ),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"1000.0\"/>" # nolint this is no absolute path
  )

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = Inf),
      beauti_options = create_beauti_options()
    ),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>" # nolint this is no absolute path
  )

})
