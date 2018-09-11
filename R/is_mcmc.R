#' Determine if the object is a valid MCMC
#' @param x an object, to be determined if it is a valid MCMC
#' @seealso Use \code{\link{create_mcmc}}
#' @return TRUE if x is a valid MCMC, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @noRd
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
#'   * [1] Maturana, Patricio, et al. "Model selection and parameter inference
#'     in phylogenetics using Nested Sampling."
#'     arXiv preprint arXiv:1703.05471 (2017).
#' @noRd
is_mcmc_nested_sampling <- function(
  x
) {
  if (!is_mcmc(x)) return(FALSE)
  if (!"particle_count" %in% names(x)) return(FALSE)
  if (!"sub_chain_length" %in% names(x)) return(FALSE)
  if (!"epsilon" %in% names(x)) return(FALSE)
  if (x$particle_count < 1) return(FALSE)
  if (x$sub_chain_length < 1) return(FALSE)
  if (x$epsilon <= 0.0) return(FALSE)
  TRUE
}
