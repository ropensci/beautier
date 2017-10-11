#' Determines if the name is a valid tree prior name
#' @param name the name to be tested
#' @return TRUE if the name is a valid tree_prior name, FALSE otherwise
#' @export
is_tree_prior_name <- function(name) {
  return(name %in% beastscriptr::get_tree_prior_names())
}
