#' Extract the MCMC chain length from an mcmc object.
#' @inheritParams default_params_doc
#' @return the MCMC chain length
#' @author Richel J.C. Bilderbeek
#' @export
get_mcmc_chain_length <- function(mcmc) {

  if (!is_mcmc(mcmc)) {
    stop("mcmc must be an mcmc object")
  }
  testit::assert("chain_length" %in% names(mcmc))
  mcmc$chain_length
}
