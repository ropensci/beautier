#' Create an MCMC configuration.
#'
#' Create an MCMC configuration, as in the BEAUti MCMC tab.
#'
#' There are four things that can be saved:
#'  * \code{store_every}: saves the state of the MCMC to file,
#'    as a \code{.state.xml} file
#'  * \code{tracelog}: stores the trace of the state of the MCMC
#'    to file. See \code{create_tracelog}
#'    how to specify the filename
#'  * \code{screenlog}: stores the screen output
#'    to file. See \code{create_screenlog}
#'    how to specify the filename
#'  * \code{treelog}: stores the estimated phylogenies
#'    to file. See \code{create_treelog}
#'    how to specify the filename
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
#' check_empty_beautier_folder()
#'
#' # Create an MCMC chain with 50 states
#' mcmc <- create_mcmc(chain_length = 50000, store_every = 1000)
#'
#' beast2_input_file <- get_beautier_tempfilename()
#' create_beast2_input_file(
#'   get_fasta_filename(),
#'   beast2_input_file,
#'   mcmc = mcmc
#' )
#' file.remove(beast2_input_file)
#'
#' remove_beautier_folder()
#' check_empty_beautier_folder()
#' @export
create_mcmc <- function(
  chain_length = 10000000,
  store_every = -1,
  pre_burnin = 0,
  n_init_attempts = 10,
  sample_from_prior = FALSE,
  tracelog = beautier::create_tracelog(),
  screenlog = beautier::create_screenlog(),
  treelog = beautier::create_treelog()
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
