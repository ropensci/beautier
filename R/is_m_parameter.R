#' Determine if the object is a valid
#' m parameter
#' @param x an object, to be determined if it is a valid
#'   m parameter
#' @return TRUE if x is a valid m parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_m_parameter <- function(
  x
) {
  if (!beautier::is_parameter(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "m")
}
