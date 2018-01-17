#' Function to create the MCMC options, as in the BEAUti MCMC tab.
#' @param chain_length the MCMC's chain length
#' @return an mcmc
#' @author Richel J.C. Bilderbeek
#' @examples
#'   mcmc <- create_mcmc(chain_length = 50000)
#'
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     "create_mcmc.xml",
#'     mcmc = mcmc
#'   )
#'   testit::assert(file.exists("create_mcmc.xml"))
#' @export
create_mcmc <- function(
  chain_length = 10000000
) {
  if (chain_length <= 0) {
    stop("chain_length must be positive and non-zero")
  }
  mcmc <- list(
    chain_length = chain_length
  )

  # Postcondition
  testit::assert(is_mcmc(mcmc))
  mcmc
}
