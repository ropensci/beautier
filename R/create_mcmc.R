#' Create an MCMC configuration.
#'
#' Create an MCMC configuration, as in the BEAUti MCMC tab.
#' The number of states that will be saved equals the chain
#' length (\code{chain_length}) divided by the number of
#' states between each sampling event (\code{store_every})
#' @inheritParams default_params_doc
#' @return an MCMC configuration
#' @seealso
#' Use \code{\link{create_test_mcmc}} to create a short regular MCMC,
#' that can be used for testing runs.
#' Use \code{\link{create_ns_mcmc}} to create an MCMC for a Nested Sampling run.
#' Use \code{\link{check_mcmc}} to check if an MCMC is valid.
#' Use \code{\link{rename_mcmc_filenames}} to rename the filenames in an MCMC.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' # Create an MCMC chain with 50 states
#' mcmc <- create_mcmc(chain_length = 50000, store_every = 1000)
#'
#' beast2_input_file <- tempfile()
#' create_beast2_input_file(
#'   get_fasta_filename(),
#'   beast2_input_file,
#'   mcmc = mcmc
#' )
#' @export
create_mcmc <- function(
  chain_length = 10000000,
  store_every = -1,
  pre_burnin = 0,
  n_init_attempts = 10,
  sample_from_prior = FALSE,
  tracelog = create_tracelog(),
  screenlog = create_screenlog(),
  treelog = create_treelog()
) {
  mcmc <- list(
    chain_length = chain_length,
    store_every = store_every,
    pre_burnin = pre_burnin,
    n_init_attempts = n_init_attempts,
    sample_from_prior = sample_from_prior,
    tracelog = tracelog,
    screenlog = screenlog,
    treelog = treelog
  )

  # Postcondition
  beautier::check_mcmc(mcmc)
  mcmc
}
