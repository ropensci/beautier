#' Determine if the object is a valid GTR site model
#' @param x an object, to be determined if it is a valid GTR site model
#' @return TRUE if x is a valid GTR site model, FALSE otherwise
#' @author Richel Bilderbeek
#' @export
is_gtr_site_model <- function(
  x
) {
  return("name" %in% names(x) && x$name == "GTR")
}
