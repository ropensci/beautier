#' Extract the MCMC chain length from an mcmc object.
#' @param mcmc one mcmc, as created
#'   by \code{\link{create_mcmc}}
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
