#' Determine if the object is a valid
#' coalescent exponential population tree prior
#' @param x an object, to be determined if it is a valid
#'   constant coalescent exponential population tree prior
#' @return TRUE if x is a valid coalescent exponentialpopulation tree prior,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_cep_tree_prior <- function(
  x
) {
  return("name" %in% names(x) && x$name == "coalescent_exponential_population")
}
