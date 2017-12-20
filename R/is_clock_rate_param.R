#' Determine if the object is a valid
#' clock_rate parameter
#' @param x an object, to be determined if it is a valid
#'   clock_rate parameter
#' @return TRUE if x is a valid clock_rate parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_clock_rate_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE)
  "name" %in% names(x) && x$name == "clock_rate"
}
