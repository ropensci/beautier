#' Determine if the object is a valid constant coalescence population tree prior
#' @param x an object, to be determined if it is a valid
#'   constant coalescence population tree prior
#' @return TRUE if x is a valid constant coalescence population tree prior,
#'   FALSE otherwise
#' @author Richel Bilderbeek
#' @export
is_ccp_tree_prior <- function(
  x
) {
  return("name" %in% names(x) && x$name == "coalescent_constant_population")
}
