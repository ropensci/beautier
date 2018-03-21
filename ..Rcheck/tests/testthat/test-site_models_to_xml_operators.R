context("site_models_to_xml_operators")

################################################################################
# Single aligment
################################################################################

test_that("GTR", {

  expected <- c(
    "<operator id=\"RateACScaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@rateAC.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateAGScaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@rateAG.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateATScaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@rateAT.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateCGScaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@rateCG.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateGTScaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@rateGT.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:test_output_0\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:test_output_0\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_gtr_site_model(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("HKY", {

  expected <- c(
    "<operator id=\"KappaScaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@kappa.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:test_output_0\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:test_output_0\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_models = list(
      create_hky_site_model(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("JC69", {

  testthat::expect_true(
    is.null(
      beautier:::site_models_to_xml_operators(
        site_models = list(create_jc69_site_model()) # No ID needed
      )
    )
  )
})

test_that("TN93", {

  expected <- c(
    "<operator id=\"kappa1Scaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@kappa1.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"kappa2Scaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@kappa2.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:test_output_0\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:test_output_0\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_models = list(
      create_tn93_site_model(id = "test_output_0")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments, unlinked site models
################################################################################

test_that("GTR GTR", {

  expected <- c(
    "<operator id=\"RateACScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateAC.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateAGScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateAG.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateATScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateAT.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateCGScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateCG.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateGTScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateGT.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateACScaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@rateAC.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateAGScaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@rateAG.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateATScaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@rateAT.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateCGScaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@rateCG.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateGTScaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@rateGT.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:anthus_nd2\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_nd2\"/>", # nolint XML
    "</operator>",
    "<operator id=\"FrequenciesExchanger.s:anthus_aco\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_aco\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_gtr_site_model(id = "anthus_aco"),
      create_gtr_site_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("HKY HKY", {

  expected <- c(
    "<operator id=\"KappaScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@kappa.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"KappaScaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@kappa.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:anthus_aco\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_aco\"/>", # nolint XML
    "</operator>",
    "<operator id=\"FrequenciesExchanger.s:anthus_nd2\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_nd2\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_hky_site_model(id = "anthus_aco"),
      create_hky_site_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("JC69 JC69", {

  testthat::expect_true(
    is.null(
      beautier:::site_models_to_xml_operators(
        site_model = list(
          create_jc69_site_model(id = "anthus_aco"),
          create_jc69_site_model(id = "anthus_nd2")
        )
      )
    )
  )
})

test_that("TN93 TN93", {

  expected <- c(
    "<operator id=\"kappa1Scaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@kappa1.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"kappa2Scaler.s:anthus_nd2\" spec=\"ScaleOperator\" parameter=\"@kappa2.s:anthus_nd2\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"kappa1Scaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@kappa1.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"kappa2Scaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@kappa2.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:anthus_nd2\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_nd2\"/>", # nolint XML
    "</operator>",
    "<operator id=\"FrequenciesExchanger.s:anthus_aco\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_aco\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_tn93_site_model(id = "anthus_aco"),
      create_tn93_site_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments, linked site models
################################################################################

test_that("GTR shared", {

  expected <- c(
    "<operator id=\"RateACScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateAC.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateAGScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateAG.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateATScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateAT.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateCGScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateCG.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"RateGTScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@rateGT.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:anthus_aco\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_aco\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_gtr_site_model(id = "anthus_aco"),
      create_gtr_site_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("shared HKY", {

  expected <- c(
    "<operator id=\"KappaScaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@kappa.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:anthus_aco\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_aco\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_hky_site_model(id = "anthus_aco"),
      create_hky_site_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("shared JC69", {

  expected <- c(
    # Nothing
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("shared TN93", {

  expected <- c(
    "<operator id=\"kappa1Scaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@kappa1.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"kappa2Scaler.s:anthus_aco\" spec=\"ScaleOperator\" parameter=\"@kappa2.s:anthus_aco\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:anthus_aco\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:anthus_aco\"/>", # nolint XML
    "</operator>"
  )
  created <- beautier:::site_models_to_xml_operators(
    site_model = list(
      create_tn93_site_model(id = "anthus_aco"),
      create_tn93_site_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
