context("clock_models_to_xml_operators")

test_that("strict", {

  created <- clock_models_to_xml_operators(
    clock_models = list(
      create_strict_clock_model()
    )
  )
  testthat::expect_true(is.null(created))
})

test_that("RLN", {

  expected <- c(
    "<operator id=\"ucldStdevScaler.c:test_output_0\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:test_output_0\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:test_output_0\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:test_output_0\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:test_output_0\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\"/>" # nolint XML
  )
  created <- clock_models_to_xml_operators(
    clock_model = list(
      create_rln_clock_model(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})
