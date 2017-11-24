#' Determine if the object is a valid
#' mu parameter
#' @param x an object, to be determined if it is a valid
#'   mu parameter
#' @return TRUE if x is a valid mu parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_muparam <- function(
  x
) {
  if (!beautier::isparam(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "mu")
}
