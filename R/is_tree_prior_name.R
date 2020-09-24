#' Determines if the name is a valid tree prior name
#' @param name the name to be tested
#' @return TRUE if the name is a valid tree_prior name, FALSE otherwise
#' @examples
#' # TRUE
#' is_tree_prior_name("birth_death")
#' is_tree_prior_name("coalescent_bayesian_skyline")
#' is_tree_prior_name("coalescent_constant_population")
#' is_tree_prior_name("coalescent_exp_population")
#' is_tree_prior_name("yule")
#' # FALSE
#' is_tree_prior_name("nonsense")
#' @author Richèl J.C. Bilderbeek
#' @export
is_tree_prior_name <- function(name) {
  name %in% beautier::get_tree_prior_names()
}
