#' Determine if the object is a valid
#' beta parameter
#' @param x an object, to be determined if it is a valid
#'   beta parameter
#' @return TRUE if x is a valid beta parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_beta_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  "name" %in% names(x) && x$name == "beta"
}
