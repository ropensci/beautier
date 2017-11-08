context("distribution_to_xml")

test_that("add upper", {

  testthat::expect_equal(
    distribution_to_xml(distribution = create_uniform_distr(id = 1, upper = NA)),
    "<Uniform id=\"Uniform.1\" name=\"distr\"/>"
  )

  testthat::expect_equal(
    distribution_to_xml(distribution = create_uniform_distr(id = 1, upper = "1000.0")),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"1000.0\"/>"
  )

  testthat::expect_equal(
    distribution_to_xml(distribution = create_uniform_distr(id = 1, upper = Inf)),
    "<Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>"
  )

})
