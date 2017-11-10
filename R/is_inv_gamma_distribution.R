#' Determine if the object is a valid
#' inv_gamma distribution,
#' as created by \code{\link{create_inv_gamma_distr}}
#' @param x an object, to be determined if it is a valid
#'   inv_gamma distribution
#' @return TRUE if x is a valid inv_gamma distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distribution}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @export
is_inv_gamma_distr <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "inv_gamma")
}
