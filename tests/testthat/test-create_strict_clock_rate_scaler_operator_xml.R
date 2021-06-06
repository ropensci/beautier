test_that("use", {
  expect_silent(
    create_strict_clock_rate_scaler_operator_xml(
      inference_model = create_inference_model(
        clock_model = create_strict_clock_model(id = 314)
      )
    )
  )
})

test_that("detailed use, v2.4", {
  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("anthus_aco_sub_calibrated.xml")),
      "StrictClockRateScaler"
    )
  )
  expect_equal(
    create_strict_clock_rate_scaler_operator_xml(
      inference_model = create_inference_model(
        clock_model = create_strict_clock_model(id = "anthus_aco_sub")
      )
    ),
    expected
  )
})

test_that("detailed use, v2.6", {
  expected <- unindent(
    stringr::str_subset(
      readr::read_lines(get_beautier_path("tipdates_2_6.xml")),
      "StrictClockRateScaler"
    )
  )
  expect_equal(
    create_strict_clock_rate_scaler_operator_xml(
      inference_model = create_inference_model(
        clock_model = create_strict_clock_model(id = "test_output_0"),
        beauti_options = create_beauti_options_v2_6()
      )
    ),
    expected
  )
})
