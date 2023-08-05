test_that("usage", {

  input_filename <- get_beautier_path("anthus_aco.fas")
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

test_that("v2.4", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_beast2_input_run(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expect_true(
    created[1] == "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"10000000\">" ||
      created[1] == "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1e+07\">"
  )
  expect_equal(length(created), 76)
  expect_equal(
    created[76],
    "</run>"
  )
})

test_that("v2.6", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_beast2_input_run(
    input_filename = input_filename,
    inference_model = inference_model
  )
  # New in v2.6
  expect_equal(3, length(stringr::str_subset(created, "spec=\"Logger\" ")))
  expect_equal(1, length(stringr::str_subset(created, "OperatorSchedule")))
})

test_that("abuse", {

  expect_error(
    create_beast2_input_run(
      input_filename = c("a", "b")
    )
  )
  expect_error(
    create_beast2_input_run(
      input_filename = get_beautier_path("anthus_aco.fas"),
      inference_model = "nonsense"
    )
  )
})
