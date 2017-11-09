#' Determine if the object is a valid parameter
#' @param x an object, to be determined if it is a valid
#'   parameter
#' @return TRUE if x is a valid parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_parameter <- function(
  x
) {
  return("name" %in% names(x))
}
