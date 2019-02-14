#' Determine if the object is a valid MCMC
#' @param x an object, to be determined if it is a valid MCMC
#' @seealso Use \code{\link{create_mcmc}}
#' @return TRUE if x is a valid MCMC, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_mcmc <- function(
  x
) {
  if (!"chain_length" %in% names(x)) return(FALSE)
  if (x$chain_length <= 0) return(FALSE)
  if (!"store_every" %in% names(x)) return(FALSE)
  if (!is.na(x$store_every) && x$store_every < -1) return(FALSE)
  if (!is.na(x$store_every) && x$store_every == 0) return(FALSE)
  if (!is.na(x$store_every) && x$store_every > x$chain_length) return(FALSE)
  TRUE
}

#' Determine if the object is a valid Nested-Sampling MCMC,
#'   as used in [1]
#' @param x an object, to be determined if it is a valid MCMC
#' @seealso Use \code{\link{create_mcmc_nested_sampling}}
#' @return TRUE if x is a valid Nested-Sampling MCMC, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @references
#'   * [1] Patricio Maturana Russel, Brendon J Brewer, Steffen Klaere,
#'     Remco R Bouckaert; Model Selection and Parameter Inference in
#'     Phylogenetics Using Nested Sampling, Systematic Biology, 2018,
#'     syy050, https://doi.org/10.1093/sysbio/syy050
#' @export
#' @aliases is_mcmc_nested_sampling is_nested_sampling_mcmc
#' @export is_mcmc_nested_sampling is_nested_sampling_mcmc
is_mcmc_nested_sampling <- is_nested_sampling_mcmc <- function(
  x
) {
  if (!is_mcmc(x)) return(FALSE) # nolint beautier function
  if (!"particle_count" %in% names(x)) return(FALSE)
  if (!"sub_chain_length" %in% names(x)) return(FALSE)
  if (!"epsilon" %in% names(x)) return(FALSE)
  if (x$particle_count < 1) return(FALSE)
  if (x$sub_chain_length < 1) return(FALSE)
  if (x$epsilon <= 0.0) return(FALSE)
  TRUE
}
