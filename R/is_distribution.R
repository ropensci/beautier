#' Determine if the object is a valid distribution
#' @param x an object, to be determined if it is a valid
#'   distribution
#' @return TRUE if x is a valid distribution,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_distribution <- function(
  x
) {
  return("name" %in% names(x))
}
