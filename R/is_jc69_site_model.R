#' Determine if the object is a valid JC69 site model
#' @param x an object, to be determined if it is a valid JC69 site model
#' @return TRUE if x is a valid JC69 site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_jc69_site_model <- function(
  x
) {
  return("name" %in% names(x) && x$name == "JC69")
}
