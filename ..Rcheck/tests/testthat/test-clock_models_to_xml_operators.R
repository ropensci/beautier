context("clock_models_to_xml_operators")

################################################################################
# Single aligment
################################################################################

test_that("RLN", {

  expected <- c(
    "<operator id=\"ucldStdevScaler.c:test_output_0\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:test_output_0\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:test_output_0\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:test_output_0\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:test_output_0\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:test_output_0\" weight=\"10.0\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_model = list(
      create_rln_clock_model(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict", {

  created <- beautier:::clock_models_to_xml_operators(
    clock_models = list(
      create_strict_clock_model()
    )
  )
  testthat::expect_true(is.null(created))
})

################################################################################
# Two alignments, unlinked clock models
################################################################################

test_that("RLN RLN", {

  expected <- c(
    "<operator id=\"ucldStdevScaler.c:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_aco\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_aco\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_aco\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_aco\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"ucldMeanScaler.c:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@ucldMean.c:anthus_nd2\" scaleFactor=\"0.5\" weight=\"1.0\"/>", # nolint XML
    "<operator id=\"ucldStdevScaler.c:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_nd2\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_nd2\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_nd2\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_nd2\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_nd2\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_nd2\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_nd2\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"relaxedUpDownOperator.c:anthus_nd2\" spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">", # nolint XML
    "    <up idref=\"ucldMean.c:anthus_nd2\"/>", # nolint XML
    "    <down idref=\"Tree.t:anthus_nd2\"/>", # nolint XML
    "</operator>" # nolint XML

  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_model = list(
      create_rln_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("RLN strict", {

  expected <- c(
    "<operator id=\"StrictClockRateScaler.c:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@clockRate.c:anthus_nd2\" scaleFactor=\"0.75\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"strictClockUpDownOperator.c:anthus_nd2\" spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">", # nolint XML
    "    <up idref=\"clockRate.c:anthus_nd2\"/>", # nolint XML
    "    <down idref=\"Tree.t:anthus_nd2\"/>", # nolint XML
    "</operator>", # nolint XML
    "<operator id=\"ucldStdevScaler.c:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_aco\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_aco\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_aco\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_aco\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_model = list(
      create_rln_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict RLN", {

  expected <- c(
    "<operator id=\"ucldMeanScaler.c:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@ucldMean.c:anthus_nd2\" scaleFactor=\"0.5\" weight=\"1.0\"/>", # nolint XML
    "<operator id=\"ucldStdevScaler.c:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_nd2\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_nd2\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_nd2\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_nd2\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_nd2\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_nd2\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_nd2\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"relaxedUpDownOperator.c:anthus_nd2\" spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">", # nolint XML
    "    <up idref=\"ucldMean.c:anthus_nd2\"/>", # nolint XML
    "    <down idref=\"Tree.t:anthus_nd2\"/>", # nolint XML
    "</operator>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_model = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict strict", {

  expected <- c(
    "<operator id=\"StrictClockRateScaler.c:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@clockRate.c:anthus_nd2\" scaleFactor=\"0.75\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"strictClockUpDownOperator.c:anthus_nd2\" spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">", # nolint XML
    "    <up idref=\"clockRate.c:anthus_nd2\"/>", # nolint XML
    "    <down idref=\"Tree.t:anthus_nd2\"/>", # nolint XML
    "</operator>"

  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_model = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments, linked clock models
################################################################################

test_that("RLN shared", {

  expected <- c(
    "<operator id=\"ucldStdevScaler.c:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@ucldStdev.c:anthus_aco\" scaleFactor=\"0.5\" weight=\"3.0\"/>", # nolint XML
    "<operator id=\"CategoriesRandomWalk.c:anthus_aco\" spec=\"IntRandomWalkOperator\" parameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\" windowSize=\"1\"/>", # nolint XML
    "<operator id=\"CategoriesSwapOperator.c:anthus_aco\" spec=\"SwapOperator\" intparameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\"/>", # nolint XML
    "<operator id=\"CategoriesUniform.c:anthus_aco\" spec=\"UniformOperator\" parameter=\"@rateCategories.c:anthus_aco\" weight=\"10.0\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_model = list(
      create_rln_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("shared strict", {

  expected <- c(
    # Nothing
  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_model = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
