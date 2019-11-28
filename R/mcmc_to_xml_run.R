#' Converts an MCMC object to the run section's XML
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#' library(testthat)
#'
#' xml <- mcmc_to_xml_run(create_mcmc())
#' expect_equal(
#'   xml,
#'   "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1e+07\">"
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
mcmc_to_xml_run <- function(mcmc) {
  testit::assert(beautier::is_mcmc(mcmc))
  if (beautier::is_default_mcmc(mcmc)) {
    beautier::mcmc_to_xml_run_default(mcmc)
  } else {
    testit::assert(beautier::is_mcmc_nested_sampling(mcmc))
    beautier::mcmc_to_xml_run_nested_sampling(mcmc)
  }
}

#' Converts an MCMC object to the run section's XML for a default MCMC
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#' library(testthat)
#'
#' xml <- mcmc_to_xml_run_default(create_mcmc())
#'
#' expect_equal(
#'   xml,
#'   "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1e+07\">"
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
mcmc_to_xml_run_default <- function(mcmc) {
  testit::assert(beautier::is_mcmc(mcmc))
  testit::assert(beautier::is_default_mcmc(mcmc))
  xml <- paste0(
    "<run id=\"mcmc\" spec=\"MCMC\" ",
    "chainLength=\"", mcmc$chain_length, "\""
  )

  if (mcmc$n_init_attempts != 10) {
    xml <- paste0(
      xml,
      " numInitializationAttempts=\"", mcmc$n_init_attempts, "\""
    )
  }

  if (mcmc$pre_burnin > 0) {
    xml <- paste0(xml, " preBurnin=\"", mcmc$pre_burnin, "\"")
  }

  if (mcmc$sample_from_prior == TRUE) {
    xml <- paste0(xml, " sampleFromPrior=\"",
      tolower(as.character(mcmc$sample_from_prior)), "\"")
  }


  if (!beautier::is_one_na(mcmc$store_every) && mcmc$store_every > 0) {
    xml <- paste0(xml, " storeEvery=\"", mcmc$store_every, "\"")
  }
  xml <- paste0(xml, ">")
  xml
}

#' Converts an MCMC object to the run section's XML for a Nested-Sampling MCMC
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#' library(testthat)
#'
#' xml <- mcmc_to_xml_run_nested_sampling(
#'   create_ns_mcmc()
#' )
#'
#' expect_equal(
#'   xml,
#'   paste0(
#'     "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"1e+07\" ",
#'     "particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">"
#'   )
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
mcmc_to_xml_run_nested_sampling <- function(mcmc) { # nolint beautier function can be long
  testit::assert(beautier::is_mcmc(mcmc))
  testit::assert(beautier::is_mcmc_nested_sampling(mcmc))
  xml <- paste0(
    "<run id=\"mcmc\" spec=\"beast.gss.NS\" ",
    "chainLength=\"", mcmc$chain_length, "\""
  )
  if (!beautier::is_one_na(mcmc$store_every) && mcmc$store_every > 0) {
    xml <- paste0(xml, " storeEvery=\"", mcmc$store_every, "\"")
  }
  xml <- paste0(xml,
    " particleCount=\"", mcmc$particle_count, "\" ",
    "subChainLength=\"", mcmc$sub_chain_length, "\" ",
    "epsilon=\"", mcmc$epsilon, "\"",
    ">"
  )
  xml
}
