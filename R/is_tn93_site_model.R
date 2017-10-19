#' Determine if the object is a valid TN93 site model
#' @param x an object, to be determined if it is a valid TN93 site model
#' @return TRUE if x is a valid TN93 site model, FALSE otherwise
#' @author Richel Bilderbeek
#' @export
is_tn93_site_model <- function(
  x
) {
  return("name" %in% names(x) && x$name == "TN93")
}
