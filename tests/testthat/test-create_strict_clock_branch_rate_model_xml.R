test_that("use", {
  expect_equal(
    3,
    length(
      create_strict_clock_branch_rate_model_xml(
        inference_model = create_inference_model(
          clock_model = create_strict_clock_model(id = "anthus_aco_sub")
        )
      )
    )
  )
  expect_equal(
    1,
    length(
      create_strict_clock_branch_rate_model_xml(
        inference_model = create_inference_model(
          clock_model = create_strict_clock_model(
            id = "anthus_aco_sub",
            clock_rate_param = create_clock_rate_param(estimate = "TRUE")
          )
        )
      )
    )
  )
})
