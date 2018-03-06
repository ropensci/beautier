#' Converts an MCMC object to the run section's XML
#' @inheritParams default_params_doc
#' @return the XML as text
#' @author Richel J.C. Bilderbeek
#' @examples
#'   xml <- beautier:::mcmc_to_xml_run(create_mcmc())
#'   testit::assert(xml ==
#'     "<run id=\"mcmc\" spec=\"MCMC\" chainLength=\"10000000\">"
#'   )
mcmc_to_xml_run <- function(mcmc) {
  testit::assert(is_mcmc(mcmc))
  xml <- paste0("<run id=\"mcmc\" spec=\"MCMC\" ",
    "chainLength=\"", mcmc$chain_length, "\"")
  if (!is.na(mcmc$store_every) && mcmc$store_every > 0) {
    xml <- paste0(xml, " storeEvery=\"", mcmc$store_every, "\"")
  }
  xml <- paste0(xml, ">")
  xml
}
