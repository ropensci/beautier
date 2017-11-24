#' Determine if the object is a valid
#' m parameter
#' @param x an object, to be determined if it is a valid
#'   m parameter
#' @return TRUE if x is a valid m parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_mparam <- function(
  x
) {
  if (!beautier::is_param(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "m")
}
