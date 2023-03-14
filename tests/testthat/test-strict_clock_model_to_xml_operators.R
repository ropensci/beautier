test_that("strict", {
  created <- strict_clock_model_to_xml_operators(
    inference_model = create_inference_model()
  )
  expect_true(is.null(created))
})

test_that("strict, estimate clock rate parameter", {

  inference_model <- create_inference_model(
    clock_model = beautier::create_strict_clock_model(
      id = "anthus_aco_sub",
      clock_rate_param = beautier::create_clock_rate_param(
        value = 0.1,
        estimate = TRUE
      ),
      clock_rate_distr = beautier::create_uniform_distr(
        value = 0.1, lower = 0.0, upper = 1.0
      )
    )
  )
  created <- strict_clock_model_to_xml_operators(
    inference_model = inference_model
  )
  expect_equal(5, length(created))
})
