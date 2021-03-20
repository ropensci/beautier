test_that("use", {

  output_filename <- tempfile()
  testit::assert(!file.exists(output_filename))

  expect_silent(
    create_beast2_input_file_from_model(
      get_fasta_filename(),
      output_filename
    )
  )

  expect_true(file.exists(output_filename))
})

test_that("abuse", {

  output_filename <- tempfile()

  # input_filenames
  expect_error(
    create_beast2_input_file_from_model(
      input_filename = "nonexisting", # Error
      output_filename
    ),
    "'input_filename' not found"
  )

  # 2 site_model
  expect_error(
    create_beast2_input_file_from_model(
      input_filename = get_fasta_filename(),
      output_filename,
      inference_model = "nonsense"
    ),
    "'inference_model' must be an inference model"
  )
})

test_that("cannot create CBS with less than 6 taxa", {
  # Tested by 'check_file_and_model_agree'
})

test_that("https://github.com/ropensci/beautier/issues/121", {

  inference_model <- create_inference_model(
    tree_prior = create_ccp_tree_prior(
      pop_size_distr = create_normal_distr(
        lower = "12.0",
        upper = "345.0",
        value = "100.0"
      )
    ),
    beauti_options = create_beauti_options_v2_6()
  )

  text <- create_beast2_input_from_model(
    get_beautier_path("anthus_nd2_sub.fas"),
    inference_model
  )

  expected <- paste0(
    "            ",
    "<parameter id=\"popSize.t:anthus_nd2_sub\" ",
      "spec=\"parameter.RealParameter\" lower=\"12.0\" name=\"stateNode\" ",
      "upper=\"345.0\">",
      "100.0",
    "</parameter>"
  )
  created <- stringr::str_subset(text, "popSize.t:anthus_nd2_sub.*stateNode")
  expect_equal(expected, created)
})
