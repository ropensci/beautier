context("create_beast2_input_package")

# Create BEAST2 input files that employ a BEAST2 package
################################################################################
# NS: nested sampling
################################################################################
test_that("NS, shallow", {
  normal <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco.fas"),
    mcmc = create_ns_mcmc()
  )
  matches <- stringr::str_match(string = normal, pattern = ".*run id=.*")
  expect_equal(
    matches[!is.na(matches)],
    "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"10000000\" particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">" # nolint long line indeed
  )
})

test_that("NS, deep", {

  # We cannot deeply test if the BEAST2 input file is valid:
  # for that, we need beastier.
  # Therefore, this test is done in
  # mauricer's tests/testthat/test-create_beast2_input_package.R
})
