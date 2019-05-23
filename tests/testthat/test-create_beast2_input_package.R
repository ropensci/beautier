context("create_beast2_input_package")

# Create BEAST2 input files that employ a BEAST2 package

################################################################################
# NS: nested sampling
################################################################################
test_that("NS, shallow", {
  normal <- create_beast2_input_from_model(
  input_filename = get_beautier_path("anthus_aco.fas"),
  inference_model = create_inference_model(
      mcmc = create_mcmc_nested_sampling()
    )
  )
  matches <- stringr::str_match(string = normal, pattern = ".*run id=.*")
  line <- matches[!is.na(matches)]
  testit::assert(line == "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"10000000\" particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">") # nolint
})

test_that("NS, deep", {

  return()

  # Cannot check a BEAST2 input file to be valid
  # when it is using a construct of the NS package
  output_filename <- tempfile()

  create_beast2_input_file_from_model(
    input_filename = get_beautier_path("anthus_aco.fas"),
    create_inference_model(
      mcmc = create_mcmc_nested_sampling()
    ),
    output_filename = output_filename
  )
  expect_false(
    beastier::is_beast2_input_file(
      output_filename,
      show_warnings = FALSE,
      verbose = TRUE
    )
  )
})
