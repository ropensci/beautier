#' Determine if the object is a valid strict clock model
#' @param x an object, to be determined if it is a valid strict clock model
#' @return TRUE if x is a valid strict clock model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_strict_clock_model <- function(
  x
) {
  return("name" %in% names(x) && x$name == "strict")
}
