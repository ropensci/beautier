#' Function to create the MCMC options, as in the BEAUti MCMC tab.
#' @param chain_length the MCMC's chain length
#' @param store_every number of states the posterior will be saved to file.
#'   Use -1 or NA to use the default frequency
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
  chain_length = 10000000,
  store_every = -1
) {
  if (chain_length <= 0) {
    stop("'chain_length' must be positive and non-zero")
  }
  if (!is.na(store_every) && (store_every < -1 || store_every == 0)) {
    stop("'store_every' must be non-zero positive, NA or -1")
  }
  if (!is.na(store_every) && store_every > chain_length) {
    stop("'store_every' must be equal or lower to 'chain_length'")
  }

  mcmc <- list(
    chain_length = chain_length,
    store_every = store_every
  )

  # Postcondition
  testit::assert(is_mcmc(mcmc))
  mcmc
}
