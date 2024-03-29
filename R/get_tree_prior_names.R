#' Get the tree prior names
#' @return the tree prior names
#' @seealso Use \link{create_tree_priors} to get all tree priors
#' @examples
#' check_empty_beautier_folder()
#'
#' get_tree_prior_names()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_tree_prior_names <- function() {
  c(
    "birth_death",
    "coalescent_bayesian_skyline",
    "coalescent_constant_population",
    "coalescent_exp_population",
    "yule"
  )
}
