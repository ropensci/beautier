#' Converts an MCMC object to the run section's XML
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#' if (is_on_ci()) {
#'
#'   check_empty_beautier_folder()
#'
#'   # <run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1e+07\">
#'   mcmc_to_xml_run(create_mcmc())
#'
#'   check_empty_beautier_folder()
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
mcmc_to_xml_run <- function(mcmc) {
  check_true(is_mcmc(mcmc))
  if (is_default_mcmc(mcmc)) {
    mcmc_to_xml_run_default(mcmc)
  } else {
    check_true(is_mcmc_nested_sampling(mcmc))
    mcmc_to_xml_run_nested_sampling(mcmc)
  }
}

#' Converts an MCMC object to the run section's XML for a default MCMC
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#' check_empty_beautier_folder()
#'
#' # <run id=\"mcmc\" spec=\"MCMC\" chainLength=\"1e+07\">
#' xml <- mcmc_to_xml_run_default(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
mcmc_to_xml_run_default <- function(mcmc) {
  check_true(is_mcmc(mcmc))
  check_true(is_default_mcmc(mcmc))
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
    xml <- paste0(
      xml, " sampleFromPrior=\"",
      tolower(as.character(mcmc$sample_from_prior)), "\""
    )
  }


  if (!is_one_na(mcmc$store_every) && mcmc$store_every > 0) {
    xml <- paste0(xml, " storeEvery=\"", mcmc$store_every, "\"")
  }
  xml <- paste0(xml, ">")
  xml
}

#' Converts an MCMC object to the run section's XML for a Nested-Sampling MCMC
#' @inheritParams default_params_doc
#' @return the XML as text
#' @examples
#' check_empty_beautier_folder()
#'
#' #  "<run id=\"mcmc\" spec=\"beast.gss.NS\" chainLength=\"1e+07\" "
#' #  "particleCount=\"1\" subChainLength=\"5000\" epsilon=\"1e-12\">"
#' mcmc_to_xml_run_nested_sampling(create_ns_mcmc())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
mcmc_to_xml_run_nested_sampling <- function(mcmc) { # nolint indeed long function name
  check_true(is_mcmc(mcmc))
  check_true(is_mcmc_nested_sampling(mcmc))
  xml <- paste0(
    "<run id=\"mcmc\" spec=\"beast.gss.NS\" ",
    "chainLength=\"", mcmc$chain_length, "\""
  )
  if (!is_one_na(mcmc$store_every) && mcmc$store_every > 0) {
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
