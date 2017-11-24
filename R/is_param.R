#' Determine if the object is a valid parameter
#' @param x an object, to be determined if it is a valid parameter,
#'   as created by \code{\link{create_param}})
#' @return TRUE if x is a valid parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_param <- function(
  x
) {
  return("name" %in% names(x))
}
