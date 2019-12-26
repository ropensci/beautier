context("site_models_to_xml_operators")

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
  created <- site_models_to_xml_operators(
    site_model = list(
      create_gtr_site_model(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("HKY", {

  expected <- c(
    "<operator id=\"KappaScaler.s:test_output_0\" spec=\"ScaleOperator\" parameter=\"@kappa.s:test_output_0\" scaleFactor=\"0.5\" weight=\"0.1\"/>", # nolint XML
    "<operator id=\"FrequenciesExchanger.s:test_output_0\" spec=\"DeltaExchangeOperator\" delta=\"0.01\" weight=\"0.1\">", # nolint XML
    "    <parameter idref=\"freqParameter.s:test_output_0\"/>", # nolint XML
    "</operator>"
  )
  created <- site_models_to_xml_operators(
    site_models = list(
      create_hky_site_model(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("JC69", {

  testthat::expect_true(
    is.null(
      site_models_to_xml_operators(
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
  created <- site_models_to_xml_operators(
    site_models = list(
      create_tn93_site_model(id = "test_output_0")
    )
  )
  testthat::expect_true(are_equivalent_xml_lines(created, expected))
})
