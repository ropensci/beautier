test_that("minimal use", {
  expect_silent(
    create_clock_rate_state_node_parameter_xml(
      create_inference_model(
        clock_model = create_strict_clock_model(id = 314),
        tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv")
      )
    )
  )
})

test_that("detailed use", {
  expect_equal(
    create_clock_rate_state_node_parameter_xml(
      create_inference_model(
        clock_model = create_strict_clock_model(id = 314),
        tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv")
      )
    ),
    "<parameter id=\"clockRate.c:314\" name=\"stateNode\">1.0</parameter>"
  )
})

test_that("abuse", {
  expect_error(
    create_clock_rate_state_node_parameter_xml(
      create_inference_model(
        clock_model = create_strict_clock_model(), # id is NA
        tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv")
      )
    ),
    "is_one_na\\(id\\)"
  )
})
