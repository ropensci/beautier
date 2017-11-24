#' Determine if the object is a valid
#' clock_rate parameter
#' @param x an object, to be determined if it is a valid
#'   clock_rate parameter
#' @return TRUE if x is a valid clock_rate parameter,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_clock_rateparam <- function(
  x
) {
  if (!beautier::isparam(x)) return(FALSE)
  return("name" %in% names(x) && x$name == "clock_rate")
}
