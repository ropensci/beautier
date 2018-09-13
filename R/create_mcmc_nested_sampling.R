#' Create an MCMC object that does nested sampling
#' @inheritParams default_params_doc
#' @param particle_count number of particles
#' @param sub_chain_length sub-chain length
#' @param epsilon epsilon
#' @return an MCMC object
#' @examples
#'   # Create an MCMC chain with 50 states
#'   mcmc <- create_mcmc_nested_sampling(
#'     chain_length = 50000,
#'     store_every = 1000,
#'     particle_count = 1,
#'     sub_chain_length = 5000,
#'     epsilon = 1e-12
#'   )
#'
#'   create_beast2_input_file(
#'     get_fasta_filename(),
#'     "create_mcmc_nested_sampling.xml",
#'     mcmc = mcmc
#'   )
#'   testit::assert(file.exists("create_mcmc_nested_sampling.xml"))
#' @references
#'   * [1] Patricio Maturana Russel, Brendon J Brewer, Steffen Klaere,
#'     Remco R Bouckaert; Model Selection and Parameter Inference in
#'     Phylogenetics Using Nested Sampling, Systematic Biology, 2018,
#'     syy050, https://doi.org/10.1093/sysbio/syy050
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
  mcmc
}
