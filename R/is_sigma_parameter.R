#' Determine if the object is a valid
#' sigma parameter
#' @param x an object, to be determined if it is a valid
#'   sigma parameter
#' @return TRUE if x is a valid sigma parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_sigma_parameter <- function(
  x
) {
  if (!beautier::is_parameter(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "sigma")
}
