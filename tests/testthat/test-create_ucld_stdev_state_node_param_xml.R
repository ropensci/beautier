test_that("minimal use", {
  expect_silent(
    create_ucld_stdev_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314)
      )
    )
  )
})

test_that("detailed use, v2.4", {
  expected <- unindent(
    readr::read_lines(get_beautier_path("rln_2_4.xml"))[42]
  )

  expect_equal(
    create_ucld_stdev_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = "test_output_0"),
        beauti_options = create_beauti_options_v2_4()
      )
    ),
    expected
  )
})

test_that("detailed use, v2.6", {
  expected <- unindent(
    readr::read_lines(get_beautier_path("rln_2_6.xml"))[63]
  )

  expect_equal(
    create_ucld_stdev_state_node_param_xml(
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
    create_ucld_stdev_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model() # id is NA
      )
    ),
    "is_one_na\\(id\\)"
  )
})
