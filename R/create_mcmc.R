#' Function to create the MCMC options, as in the BEAUti MCMC tab.
#' @param chain_length the MCMC's chain length
#' @return an mcmc
#' @author Richel J.C. Bilderbeek
#' @export
create_mcmc <- function(
  chain_length = 10000000
) {
  if (chain_length < 10000) {
    stop("chain_length must be at least 10,000")
  }
  list(
    chain_length = chain_length
  )
}
