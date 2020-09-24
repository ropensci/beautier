#' Determine if the object is a valid MCMC
#' @param x an object, to be determined if it is a valid MCMC
#' @seealso Use \code{\link{create_mcmc}} to create an MCMC
#' @return TRUE if x is a valid MCMC, FALSE otherwise
#' @examples
#' # Returns TRUE
#' is_mcmc(create_mcmc())
#' is_mcmc(create_ns_mcmc())
#'
#' # Returns FALSE
#' is_mcmc("nonsense")
#' is_mcmc(NULL)
#' is_mcmc(NA)
#' is_mcmc("")
#' is_mcmc(c())
#' @author Richèl J.C. Bilderbeek
#' @export
is_mcmc <- function(
  x
) {
  result <- FALSE
  tryCatch({
    beautier::check_mcmc(x)
    result <- TRUE
  },
    error = function(e) {} # nolint do not care about e
  )
  result
}

#' Determine if the object is a valid Nested-Sampling MCMC,
#'   as used in [1]
#' @param x an object, to be determined if it is a valid MCMC
#' @seealso Use \link{create_ns_mcmc} to create an NS MCMC
#' @return TRUE if x is a valid Nested-Sampling MCMC, FALSE otherwise
#' @examples
#' # TRUE
#' is_nested_sampling_mcmc(create_ns_mcmc())
#' # FALSE
#' is_nested_sampling_mcmc(create_mcmc())
#' is_nested_sampling_mcmc("nonsense")
#' @author Richèl J.C. Bilderbeek
#' @references
#'   * [1] Patricio Maturana Russel, Brendon J Brewer, Steffen Klaere,
#'     Remco R Bouckaert; Model Selection and Parameter Inference in
#'     Phylogenetics Using Nested Sampling, Systematic Biology, 2018,
#'     syy050, https://doi.org/10.1093/sysbio/syy050
#' @aliases is_mcmc_nested_sampling is_nested_sampling_mcmc
#' @export is_mcmc_nested_sampling is_nested_sampling_mcmc
is_mcmc_nested_sampling <- is_nested_sampling_mcmc <- function(
  x
) {
  result <- FALSE
  tryCatch({
    check_mcmc_nested_sampling(x)
    result <- TRUE
  }, error = function(e) {} # nolint ignore error result
  )
  result
}
