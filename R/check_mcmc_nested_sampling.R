#' Check if this an MCMC that uses Nested Sampling
#' to estimate a marginal likelihood.
#'
#' Will \link{stop} if not, else will do nothing
#' @inheritParams default_params_doc
#' @return No return value, called for side effects
#' @seealso use \code{\link{create_ns_mcmc}}
#' to create an MCMC that uses Nested Sampling
#' to estimate a marginal likelihood
#' @author Richèl J.C. Bilderbeek
#' @export
#' @aliases check_ns_mcmc check_mcmc_nested_sampling check_nested_sampling_mcmc
#' @export check_ns_mcmc check_mcmc_nested_sampling check_nested_sampling_mcmc
check_ns_mcmc <- check_mcmc_nested_sampling <- check_nested_sampling_mcmc <- function(mcmc) { # nolint indeed a long line

  check_mcmc(mcmc)

  # The arguments 'chain_length' and 'store_every' are checked
  # by check_mcmc
  argument_names <- c(
    "particle_count", "sub_chain_length", "epsilon"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(mcmc)) {
      stop(
        "'", arg_name, "' must be an element of an MCMC ",
        "that uses Nested Sampling. \n",
        "Tip: use 'create_ns_mcmc'"
      )
    }
  }

  # The arguments 'chain_length' and 'store_every' are checked
  # by check_mcmc
  if (mcmc$particle_count < 1) {
    stop("'mcmc$particle_count' must be at least 1")
  }
  if (mcmc$sub_chain_length < 1) {
    stop("'mcmc$sub_chain_length' must be at least 1")
  }
  if (mcmc$epsilon <= 0.0) {
    stop("'mcmc$epsilon' must be non-zero and positive")
  }
  invisible(mcmc)
}
