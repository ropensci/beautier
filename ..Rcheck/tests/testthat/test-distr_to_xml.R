context("distr_to_xml")

test_that("add upper", {

  testthat::expect_equal(
    distr_to_xml(
        distr = create_uniform_distr(id = 1, upper = NA)),
    "<Uniform id=\"Uniform.1\" name=\"distr\"/>"
  )

  testthat::expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = "1000.0")),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"1000.0\"/>"
  )

  testthat::expect_equal(
    distr_to_xml(
      distr = create_uniform_distr(id = 1, upper = Inf)),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>"
  )

})

test_that("abuse", {

  testthat::expect_error(distr_to_xml(distr = "nonsense"))
  testthat::expect_error(distr_to_xml(distr = NA))
  testthat::expect_error(distr_to_xml(distr = NULL))

  testthat::expect_error(
    distr_to_xml(distr = create_uniform_distr(id = NA)),
    "distribution must have an ID"
  )

  testthat::expect_error(
    distr_to_xml(distr = create_laplace_distr(id = NA)),
    "distribution must have an ID"
  )

  testthat::expect_error(
    distr_to_xml(distr = create_laplace_distr(id = 1)),
    "parameter must have an ID"
  )

})
