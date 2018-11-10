context("clock_models_to_xml_state")

test_that("strict", {

  # From anthus_aco_sub.xml
  expected <- NULL # Indeed, nothing!
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(create_strict_clock_model(id = "anthus_aco_sub"))
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("rln", {

  # From rln_2_4.xml
  expected <- c(
    "<parameter id=\"ucldStdev.c:test_output_0\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>",
    "<stateNode id=\"rateCategories.c:test_output_0\" spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>"
  )
  created <- beautier:::clock_models_to_xml_state(
    clock_models = list(create_rln_clock_model(id = "test_output_0", dimension = 8))
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
