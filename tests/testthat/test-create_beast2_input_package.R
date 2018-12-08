context("create_beast2_input_package")

# Create BEAST2 input files that employ a BEAST2 package

test_that("Nested-Sampling package", {
  normal <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco.fas"),
    mcmc = create_mcmc_nested_sampling()
  )
  matches <- stringr::str_match(string = normal, pattern = ".*run id=.*")
  line <- matches[!is.na(matches)]
  testit::assert(line == "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"10000000\" particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">") # nolint
})
