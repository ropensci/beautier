test_that("minimal use, v2.4", {

  inference_model <- create_inference_model()
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expect_silent(
    create_beast2_input_operators(
      inference_model = inference_model
    )
  )
})

test_that("minimal use, v2.6", {

  inference_model <- create_inference_model(
    beauti_options = create_beauti_options_v2_6()
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expect_silent(
    create_beast2_input_operators(
      inference_model = inference_model
    )
  )
})
