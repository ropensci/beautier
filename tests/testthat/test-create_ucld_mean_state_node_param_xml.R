test_that("minimal use", {
  expect_silent(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314)
      )
    )
  )
})

test_that("details use", {
  expect_equal(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314)
      )
    ),
    "<parameter id=\"ucldMean.c:314\" name=\"stateNode\">1.0</parameter>"
  )
})

test_that("abuse", {
  expect_error(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model() # id is NA
      )
    ),
    "is_one_na\\(id\\)"
  )
  expect_error(
    create_ucld_mean_state_node_param_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(id = 314, mean_clock_rate = NA)
      )
    ),
    "is_one_na\\(mean_clock_rate\\)"
  )
})
