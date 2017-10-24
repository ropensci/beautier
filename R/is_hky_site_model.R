#' Determine if the object is a valid HKY site model
#' @param x an object, to be determined if it is a valid HKY site model
#' @return TRUE if x is a valid HKY site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_hky_site_model <- function(
  x
) {
  return("name" %in% names(x) && x$name == "HKY")
}
