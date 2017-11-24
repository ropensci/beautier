#' Determine if the object is a valid
#' s parameter
#' @param x an object, to be determined if it is a valid
#'   s parameter
#' @return TRUE if x is a valid s parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_sparam <- function(
  x
) {
  if (!beautier::isparam(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "s")
}
