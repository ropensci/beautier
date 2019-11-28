#' Create an MCMC object to estimate the marginal likelihood
#' using Nested Sampling.
#'
#' This will result in a BEAST run that estimates the marginal
#' likelihood until convergence is achieved.
#' In this context, \code{chain_length} is only an upper bound
#' to the length of that run.
#' @inheritParams default_params_doc
#' @param chain_length upper bound to the length of the MCMC chain
#' @param particle_count number of particles
#' @param sub_chain_length sub-chain length
#' @param epsilon epsilon
#' @return an MCMC object
#' @seealso
#' Use \link{create_mcmc} to create a regular MCMC.
#' Use \link{create_test_ns_mcmc} to create an NS MCMC for testing,
#'   with, among others, a short MCMC chain length.
#' Use \link{check_ns_mcmc} to check that an NS MCMC object is valid.
#' @examples
#'   mcmc <- create_ns_mcmc(
#'     chain_length = 1e7,
#'     store_every = 1000,
#'     particle_count = 1,
#'     sub_chain_length = 1000,
#'     epsilon = 1e-12
#'   )
#'
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     beast2_input_file,
#'     mcmc = mcmc
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @references
#'   * [1] Patricio Maturana Russel, Brendon J Brewer, Steffen Klaere,
#'     Remco R Bouckaert; Model Selection and Parameter Inference in
#'     Phylogenetics Using Nested Sampling, Systematic Biology, 2018,
#'     syy050, https://doi.org/10.1093/sysbio/syy050
#' @author Richèl J.C. Bilderbeek
#' @aliases create_ns_mcmc create_mcmc_nested_sampling
#' @export create_ns_mcmc create_mcmc_nested_sampling
create_ns_mcmc <- create_mcmc_nested_sampling <- function(
  chain_length = 10000000,
  store_every = -1,
  pre_burnin = 0,
  n_init_attempts = 3,
  particle_count = 1,
  sub_chain_length = 5000,
  epsilon = "1e-12",
  tracelog = create_tracelog(),
  screenlog = create_screenlog(),
  treelog = create_treelog()
) {
  # Unsure about 'sample_from_prior' in NS MCMC, Issue #108
  mcmc <- create_mcmc(
    chain_length = chain_length,
    store_every = store_every,
    pre_burnin = pre_burnin,
    n_init_attempts = n_init_attempts,
    sample_from_prior = FALSE,
    tracelog = tracelog,
    screenlog = screenlog,
    treelog = treelog
  )
  mcmc$particle_count <- particle_count
  mcmc$sub_chain_length <- sub_chain_length
  mcmc$epsilon <- epsilon
  beautier::check_nested_sampling_mcmc(mcmc)
  mcmc
}
