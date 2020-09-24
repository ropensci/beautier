#' Create an MCMC configuration for testing.
#' @inheritParams default_params_doc
#' @return an MCMC configuration
#' @seealso
#' Use \code{\link{create_mcmc}} to create a default BEAST2 MCMC
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create an MCMC chain with 50 states
#' mcmc <- create_test_mcmc()
#'
#' beast2_input_file <- tempfile(fileext = ".xml")
#' create_beast2_input_file(
#'   get_fasta_filename(),
#'   beast2_input_file,
#'   mcmc = mcmc
#' )
#' @export
create_test_mcmc <- function(
  chain_length = 3000,
  store_every = 1000,
  pre_burnin = 0,
  n_init_attempts = 10,
  sample_from_prior = FALSE,
  tracelog = create_test_tracelog(),
  screenlog = create_test_screenlog(),
  treelog = create_test_treelog()
) {
  create_mcmc(
    chain_length = chain_length,
    store_every = store_every,
    pre_burnin = pre_burnin,
    n_init_attempts = n_init_attempts,
    sample_from_prior = sample_from_prior,
    tracelog = tracelog,
    screenlog = screenlog,
    treelog = treelog
  )
}
