#' Determine if the object is a valid
#' s parameter
#' @param x an object, to be determined if it is a valid
#'   s parameter
#' @return TRUE if x is a valid s parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_s_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "s")
}
