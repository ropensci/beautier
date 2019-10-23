test_that("use", {
  expect_silent(
    create_beast2_input_loggers(
      input_filename = get_fasta_filename()
    )
  )

  # File can be absent
  expect_silent(
    create_beast2_input_loggers(
      input_filename = "abs.ent"
    )
  )

})

test_that("abuse", {

  expect_error(
    create_beast2_input_loggers(
      input_filename = c("one_too", "many_filenames")
    ),
    "input_filename"
  )
  expect_error(
    create_beast2_input_loggers(
      input_filename = get_fasta_filename(),
      inference_model = "nonsense"
    ),
    "inference_model"
  )

})
