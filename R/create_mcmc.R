#' Create an MCMC configuration.
#'
#' Create an MCMC configuration, as in the BEAUti MCMC tab.
#' The number of states that will be saved equals the chain
#' length (\code{chain_length}) divided by the number of
#' states between each sampling event (\code{store_every})
#' @param chain_length length of the MCMC chain
#' @param store_every number of states the MCMC will process
#'   before the posterior's state will be saved to file.
#'   Use -1 or NA to use the default frequency.
#' @return an MCMC configuration
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # Create an MCMC chain with 50 states
#'   mcmc <- create_mcmc(chain_length = 50000, store_every = 1000)
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
  if (!is.na(store_every) && store_every != -1 && store_every < 1000) {
    stop("'store_every' must be at least 1000, NA or -1")
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
