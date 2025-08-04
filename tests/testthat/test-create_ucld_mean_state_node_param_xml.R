test_that("minimal use", {
  expect_silent(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314),
        beauti_options = create_beauti_options_v2_6()
      )
    )
  )
})

test_that("detailed use, v2.4 RLN MRCA", {
  expect_error(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = "anthus_aco_sub"),
        beauti_options = create_beauti_options_v2_4()
      )
    ),
    "The ucldMean stateNode was absent in BEAST v2.4"
  )
})

test_that("detailed use, v2.5 RLN MRCA", {
  skip("Needs create_beauti_options_v2_5")
  expected <- "<parameter id=\"ucldMean.c:test_output_0\" name=\"stateNode\">1.0</parameter>" # nolint indeed a long line
  expected_too <- unindent(
    readr::read_lines(get_beautier_path("rln_mrca_one_div_x_2_5.xml"))[42]
  )
  expect_equal(expected, expected_too)

  expect_equal(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = "anthus_aco_sub"),
        beauti_options = create_beauti_options_v2_5()
      )
    ),
    expected
  )
})

test_that("detailed use, v2.6 RLN tipdates", {
  expected <- unindent(
    remove_empty_lines(
      readr::read_lines(get_beautier_path("rln_tipdates_2_6.xml"))
    )[33]
  )
  expect_equal(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = "test_output_0"),
        beauti_options = create_beauti_options_v2_6()
      )
    ),
    expected
  )
})

test_that("abuse", {
  expect_error(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(), # id is NA
        beauti_options = create_beauti_options_v2_6()
      )
    ),
    "is_one_na\\(id\\)"
  )
  expect_error(
    create_ucld_mean_state_node_param_xml(
      inference_model = create_inference_model(
        clock_model = create_rln_clock_model(id = 314, mean_clock_rate = NA),
        beauti_options = create_beauti_options_v2_6()
      )
    ),
    "is_one_na\\(mean_clock_rate\\)"
  )
  expect_error(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314, mean_clock_rate = 1),
        beauti_options = create_beauti_options_v2_4()
      )
    ),
    "The ucldMean stateNode was absent in BEAST v2.4"
  )

})
