context("create_beast2_input_run")

test_that("usage", {

  input_filename <- beautier::get_beautier_path("anthus_aco.fas")
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model()
  )
  expect_silent(
    create_beast2_input_run(
      input_filename = input_filename,
      inference_model = inference_model
    )
  )
})

test_that("abuse", {

  expect_error(
    create_beast2_input_run(
      input_filename = c("a", "b")
    )
  )
  expect_error(
    create_beast2_input_run(
      input_filename = beautier::get_beautier_path("anthus_aco.fas"),
      inference_model = "nonsense"
    )
  )
})
