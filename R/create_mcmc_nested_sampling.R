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
#' @seealso Use \code{\link{create_nested_sampling_mcmc}} to create a
#'   nested sampling MCMC
#' @examples
#'   mcmc <- create_mcmc_nested_sampling(
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
#' @aliases create_nested_sampling_mcmc create_mcmc_nested_sampling
#' @export create_nested_sampling_mcmc create_mcmc_nested_sampling
create_nested_sampling_mcmc <- create_mcmc_nested_sampling <- function(
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
  mcmc
}
