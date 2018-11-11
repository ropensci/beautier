
#' Converts an MCMC object to the run section's XML
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#'   xml <- beautier:::mcmc_to_xml_run(create_mcmc())
#'   testit::assert(xml ==
#'     "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"10000000\">"
#'   )
#' @author Richel J.C. Bilderbeek
#' @noRd
mcmc_to_xml_run <- function(mcmc) {
  testit::assert(is_mcmc(mcmc)) # nolint internal function
  if (is_default_mcmc(mcmc)) { # nolint internal function
    mcmc_to_xml_run_default(mcmc) # nolint internal function
  } else {
    testit::assert(is_mcmc_nested_sampling(mcmc)) # nolint internal function
    mcmc_to_xml_run_nested_sampling(mcmc) # nolint internal function
  }
}

#' Converts an MCMC object to the run section's XML for a default MCMC
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#'   xml <- beautier:::mcmc_to_xml_run_default(create_mcmc())
#'   testit::assert(xml ==
#'     "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1e+07\">"
#'   )
#' @author Richel J.C. Bilderbeek
#' @noRd
mcmc_to_xml_run_default <- function(mcmc) {
  testit::assert(is_mcmc(mcmc)) # nolint internal function
  testit::assert(is_default_mcmc(mcmc)) # nolint internal function
  xml <- paste0(
    "<run id=\"mcmc\" spec=\"MCMC\" ",
    "chainLength=\"", mcmc$chain_length, "\""
  )
  if (!is.na(mcmc$store_every) && mcmc$store_every > 0) {
    xml <- paste0(xml, " storeEvery=\"", mcmc$store_every, "\"")
  }
  xml <- paste0(xml, ">")
  xml
}

#' Converts an MCMC object to the run section's XML for a Nested-Sampling MCMC
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#'   xml <- beautier:::mcmc_to_xml_run_nested_sampling(
#'     create_mcmc_nested_sampling()
#'   )
#'   testit::assert(xml ==
#'     paste0(
#'       "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"1e+07\" ",
#'       "particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">"
#'     )
#'   )
#' @author Richel J.C. Bilderbeek
#' @noRd
mcmc_to_xml_run_nested_sampling <- function(mcmc) { # nolint internal function can be long
  testit::assert(is_mcmc(mcmc)) # nolint internal function
  testit::assert(is_mcmc_nested_sampling(mcmc)) # nolint internal function
  xml <- paste0(
    "<run id=\"mcmc\" spec=\"beast.gss.NS\" ",
    "chainLength=\"", mcmc$chain_length, "\""
  )
  if (!is.na(mcmc$store_every) && mcmc$store_every > 0) {
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
