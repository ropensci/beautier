#' Determine if the object is a valid Yule tree prior
#' @param x an object, to be determined if it is a valid Yule tree prior
#' @return TRUE if x is a valid Yule tree prior, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_yule_tree_prior <- function(
  x
) {
  return("name" %in% names(x) && x$name == "yule")
}
