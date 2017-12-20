#' Determine if the object is a valid
#' sigma parameter
#' @param x an object, to be determined if it is a valid
#'   sigma parameter
#' @return TRUE if x is a valid sigma parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_sigma_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "sigma")
}
