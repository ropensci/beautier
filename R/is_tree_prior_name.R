#' Determines if the name is a valid tree prior name
#' @param name the name to be tested
#' @return TRUE if the name is a valid tree_prior name, FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_tree_prior_name <- function(name) {
  return(name %in% beautier::get_tree_prior_names())
}
