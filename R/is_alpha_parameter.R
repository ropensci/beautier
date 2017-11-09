#' Determine if the object is a valid
#' alpha parameter
#' @param x an object, to be determined if it is a valid
#'   alpha parameter
#' @return TRUE if x is a valid alpha parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_alpha_parameter <- function(
  x
) {
  if (!beautier::is_parameter(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "alpha")
}
