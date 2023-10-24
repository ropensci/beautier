#' Create an NS MCMC object for testing
#' @inheritParams default_params_doc
#' @param chain_length upper bound to the length of the MCMC chain
#' @param particle_count number of particles
#' @param sub_chain_length sub-chain length
#' @param epsilon epsilon
#' @return an MCMC object
#' @seealso Use \code{\link{create_ns_mcmc}} to create a default
#' nested sampling MCMC
#' @examples
#' if (is_on_ci()) {
#'
#'   mcmc <- create_test_ns_mcmc()
#'   beast2_input_file <- get_beautier_tempfilename()
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     mcmc = mcmc
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_ns_mcmc <- function(
  chain_length = 2000,
  store_every = 1000,
  pre_burnin = 0,
  n_init_attempts = 3,
  particle_count = 1,
  sub_chain_length = 500,
  epsilon = 1e-12,
  tracelog = create_test_tracelog(),
  screenlog = create_test_screenlog(),
  treelog = create_test_treelog()
) {
  create_ns_mcmc(
    chain_length = chain_length,
    store_every = store_every,
    pre_burnin = pre_burnin,
    n_init_attempts = n_init_attempts,
    tracelog = tracelog,
    screenlog = screenlog,
    treelog = treelog
  )
}
