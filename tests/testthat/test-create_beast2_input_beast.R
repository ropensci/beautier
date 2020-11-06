test_that("use, v2.4, one alignment", {

  fasta_filename <- get_beautier_path("anthus_aco.fas")
  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_inference_model()
  )
  expect_silent(
    create_beast2_input_beast(
      input_filename = fasta_filename,
      inference_model = inference_model
    )
  )
})

test_that("use, v2.6, one alignment", {

  fasta_filename <- get_beautier_path("anthus_aco.fas")
  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_inference_model()
  )
  inference_model$beauti_options <- create_beauti_options_v2_6()
  expect_silent(
    create_beast2_input_beast(
      input_filename = fasta_filename,
      inference_model = inference_model
    )
  )
})

test_that("abuse", {

  expect_error(
    create_beast2_input_beast(input_filename = "nonsense")
  )

  fasta_filename_1 <- get_beautier_path("anthus_nd2.fas")
  fasta_filename_2 <- get_beautier_path("anthus_aco.fas")

  # Two filenames, one site model
  expect_error(
    create_beast2_input_beast(
      input_filename = c(fasta_filename_1, fasta_filename_2),
    )
  )
})
