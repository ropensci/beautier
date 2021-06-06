test_that("minimal use", {
  expect_silent(
    create_ucld_stdev_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314)
      )
    )
  )
})

test_that("detailed use", {
  expect_equal(
    create_ucld_stdev_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314)
      )
    ),
    "<parameter id=\"ucldStdev.c:314\" lower=\"0.0\" name=\"stateNode\">0.1</parameter>" # nolint indeed long
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
