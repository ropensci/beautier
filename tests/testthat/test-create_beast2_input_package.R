context("create_beast2_input_package")

# Create BEAST2 input files that employ a BEAST2 package

################################################################################
# NS: nested sampling
################################################################################
test_that("NS, shallow", {
  normal <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco.fas"),
    mcmc = create_mcmc_nested_sampling()
  )
  matches <- stringr::str_match(string = normal, pattern = ".*run id=.*")
  expect_equal(
    matches[!is.na(matches)],
    "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"10000000\" particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">" # nolint long line indeed
  )
})

test_that("NS, deep", {


  output_filename <- tempfile(pattern = "beast2_input_file_", fileext = ".xml")

  create_beast2_input_file_from_model(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    create_inference_model(
      mcmc = create_mcmc_nested_sampling()
    ),
    output_filename = output_filename
  )

  # Cannot check a BEAST2 input file to be valid
  # when it is using a construct of the NS package
  return()
  expect_false(
    beastier::is_beast2_input_file(
      output_filename,
      show_warnings = FALSE,
      verbose = TRUE
    )
  )
})
