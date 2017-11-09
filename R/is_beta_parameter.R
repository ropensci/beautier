#' Determine if the object is a valid
#' beta parameter
#' @param x an object, to be determined if it is a valid
#'   beta parameter
#' @return TRUE if x is a valid beta parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_beta_parameter <- function(
  x
) {
  if (!beautier::is_parameter(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "beta")
}
