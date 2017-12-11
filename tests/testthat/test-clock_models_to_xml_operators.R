context("clock_models_to_xml_operators")

test_that("strict", {

  expected <- c(
    ""
  )
  created <- beautier:::clock_models_to_xml_operators(
    clock_models = list(
      create_strict_clock_model(id = "test_output_0")
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
  beautier:::compare_lines(created, expected)
})

