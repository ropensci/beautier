context("distr_to_xml")

test_that("add upper", {

  expect_equal(
    distr_to_xml(
        distr = create_uniform_distr(id = 1, upper = NA)),
    "<Uniform id=\"Uniform.1\" name=\"distr\"/>" # nolint this is no absolute path
  )

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = "1000.0")),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"1000.0\"/>" # nolint this is no absolute path
  )

  expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = Inf)),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>" # nolint this is no absolute path
  )

})

test_that("abuse", {

  expect_error(distr_to_xml(distr = "nonsense"))
  expect_error(distr_to_xml(distr = NA))
  expect_error(distr_to_xml(distr = NULL))

  expect_error(
    distr_to_xml(distr = create_uniform_distr(id = NA)),
    "distribution must have an ID"
  )

  expect_error(
    distr_to_xml(distr = create_laplace_distr(id = NA)),
    "distribution must have an ID"
  )

  expect_error(
    distr_to_xml(distr = create_laplace_distr(id = 1))
  )

})
