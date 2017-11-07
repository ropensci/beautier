#' Determine if the object is a valid
#' inv_gamma distribution
#' @param x an object, to be determined if it is a valid
#'   inv_gamma distribution
#' @return TRUE if x is a valid inv_gamma distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_inv_gamma_distribution <- function(
  x
) {
  if (!beautier::is_distribution(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "inv_gamma")
}
