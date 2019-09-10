#' Check if this an MCMC that uses Nested Sampling
#' to estimate a marginal likelihood.
#'
#' Will \link{stop} if not, else will do nothing
#' @seealso use \link{create_nested_sampling_mcmc}
#' to create an MCMC that uses Nested Sampling
#' to estimate a marginal likelihood
#' @author Richel J.C. Bilderbeek
#' @export
check_mcmc_nested_sampling <- function(mcmc) {
  if (!is_mcmc(mcmc)) { # nolint beautier function
    stop(
      "'mcmc' must an MCMC that uses Nested Sampling. \n",
      "Tip: use 'create_nested_sampling_mcmc'"
    )
  }
  argument_names <- c(
    "chain_length", "store_every", "particle_count", "sub_chain_length",
    "epsilon"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(mcmc)) {
      stop(
        "'", arg_name, "' must be an element of an MCMC ",
        "that uses Nested Sampling. \n",
        "Tip: use 'create_nested_sampling_mcmc'"
      )
    }
  }
  if (mcmc$chain_length < 1) {
    stop("'mcmc$chain_length' must be at least 1")
  }
  if (mcmc$store_every == 0 || mcmc$store_every < -1) {
    stop("'mcmc$store_every' must be -1 or at least 1")
  }
  if (mcmc$particle_count < 1) {
    stop("'mcmc$particle_count' must be at least 1")
  }
  if (mcmc$sub_chain_length < 1) {
    stop("'mcmc$sub_chain_length' must be at least 1")
  }
  if (mcmc$epsilon <= 0.0) {
    stop("'mcmc$epsilon' must be non-zero and positive")
  }
}
