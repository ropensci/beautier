#' Function to create the MCMC options, as in the BEAUti MCMC tab.
#' @param chain_length the MCMC's chain length
#' @return an mcmc
#' @author Richel J.C. Bilderbeek
#' @export
create_mcmc <- function(
  chain_length = get_default_mcmc_chain_length()
) {
  if (chain_length <= 0) {
    stop("chain_length must be a non-zero positive integer")
  }
  list(
    chain_length = chain_length
  )
}
