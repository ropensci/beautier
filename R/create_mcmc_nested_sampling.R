#' Create an MCMC object that does nested sampling
#' @inherit default_params_doc
#' @param particle_count number of particles
#' @param sub_chain_length sub-chain length
#' @param epsilon epsilon
#' @return an MCMC object
#' @references
#'   * Maturana, Patricio, et al. "Model selection and parameter inference
#'     in phylogenetics using Nested Sampling."
#'     arXiv preprint arXiv:1703.05471 (2017).
#' @author Richel J.C. Bilderbeek
#' @export
create_mcmc_nested_sampling <- function(
  chain_length = 10000000,
  store_every = -1,
  particle_count = 1,
  sub_chain_length = 5000,
  epsilon = "1e-12"
) {
  mcmc <- create_mcmc(
    chain_length = chain_length,
    store_every = store_every
  )
  if (particle_count < 1) {
    stop("'particle_count' must be a non-zero amount")
  }
  if (sub_chain_length < 1) {
    stop("'sub_chain_length' must be a non-zero amount")
  }
  if (epsilon <= 0.0) {
    stop("'epsilon' must be a non-zero number")
  }
  mcmc$particle_count <- particle_count
  mcmc$sub_chain_length <- sub_chain_length
  mcmc$epsilon <- epsilon
  testit::assert(is_mcmc(mcmc))
  testit::assert(is_mcmc_nested_sampling(mcmc))
  mcmc
}
