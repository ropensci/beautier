#' Determine if the object is a valid
#' inv_gamma distribution,
#' as created by \code{\link{create_inv_gamma_distr}}
#' @param x an object, to be determined if it is a valid
#'   inv_gamma distribution
#' @return TRUE if x is a valid inv_gamma distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
is_inv_gamma_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "inv_gamma") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!"beta" %in% names(x)) return(FALSE)
  TRUE
}
