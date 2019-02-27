#' Create an MCMC configuration.
#'
#' Create an MCMC configuration, as in the BEAUti MCMC tab.
#' The number of states that will be saved equals the chain
#' length (\code{chain_length}) divided by the number of
#' states between each sampling event (\code{store_every})
#' @inheritParams default_params_doc
#' @return an MCMC configuration
#' @seealso
#'   \itemize{
#'     \item \link{are_equal_mcmcs} to check if two MCMCs are equal
#'   }
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   # Create an MCMC chain with 50 states
#'   mcmc <- create_mcmc(chain_length = 50000, store_every = 1000)
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     mcmc = mcmc
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @export
create_mcmc <- function(
  chain_length = 10000000,
  store_every = -1
) {
  if (chain_length <= 0) {
    stop("'chain_length' must be positive and non-zero")
  }
  if (!is_one_na(store_every) && store_every != -1 && store_every < 1000) { # nolint beautier function
    stop("'store_every' must be at least 1000, NA or -1")
  }
  if (!is_one_na(store_every) && store_every > chain_length) { # nolint beautier function
    stop("'store_every' must be equal or lower to 'chain_length'")
  }

  mcmc <- list(
    chain_length = chain_length,
    store_every = store_every
  )

  # Postcondition
  testit::assert(is_mcmc(mcmc)) # nolint beautier function
  mcmc
}
