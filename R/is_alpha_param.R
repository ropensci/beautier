#' Determine if the object is a valid
#' alpha parameter
#' @param x an object, to be determined if it is a valid
#'   alpha parameter
#' @return TRUE if x is a valid alpha parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_alpha_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  "name" %in% names(x) && x$name == "alpha"
}
