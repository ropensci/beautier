test_that("use", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    create_inference_model()
  )
  expect_silent(
    create_loggers_xml(
      input_filename = get_fasta_filename(),
      inference_model = inference_model
    )
  )
})

test_that("abuse", {

  expect_error(
    create_loggers_xml(
      input_filename = c("one_too", "many_filenames")
    ),
    "input_filename"
  )
  expect_error(
    create_loggers_xml(
      input_filename = get_fasta_filename(),
      inference_model = "nonsense"
    ),
    "inference_model"
  )

})
