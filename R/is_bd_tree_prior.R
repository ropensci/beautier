#' Determine if the object is a valid Birth Death tree prior
#'   as returned by \code{\link{create_bd_tree_prior}}
#' @param x an object, to be determined if it is a valid birth death tree prior
#' @return TRUE if x is a valid birth death tree prior, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_bd_tree_prior <- function(
  x
) {
  return("name" %in% names(x) && x$name == "birth_death")
}
