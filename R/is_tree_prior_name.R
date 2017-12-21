#' Determines if the name is a valid tree prior name
#' @param name the name to be tested
#' @return TRUE if the name is a valid tree_prior name, FALSE otherwise
#' @examples
#'   testit::assert(
#'     beautier:::is_tree_prior_name("birth_death")
#'   )
#'   testit::assert(
#'     beautier:::is_tree_prior_name("coalescent_bayesian_skyline")
#'   )
#'   testit::assert(
#'     beautier:::is_tree_prior_name("coalescent_constant_population")
#'   )
#'   testit::assert(
#'     beautier:::is_tree_prior_name("coalescent_exp_population")
#'   )
#'   testit::assert(
#'     beautier:::is_tree_prior_name("yule")
#'   )
#' @author Richel J.C. Bilderbeek
is_tree_prior_name <- function(name) {
  name %in% get_tree_prior_names() # nolint internal function
}
