#' Create a tree prior
#' @param name the tree prior name. Can be 'birth_death'
#'   or 'coalescent_constant_population'
#' @return a site_model
#' @export
create_tree_prior <- function(
  name
) {
  if (!is_tree_prior_name(name)) {
    stop("invalid tree prior name")
  }
  tree_prior <- list(name = name)
  tree_prior
}
