#' Determines if the name is a valid tree prior name
#' @param name the name to be tested
#' @return TRUE if the name is a valid tree_prior name, FALSE otherwise
#' @examples
#' library(testthat)
#'
#' expect_true(is_tree_prior_name("birth_death"))
#' expect_true(is_tree_prior_name("coalescent_bayesian_skyline"))
#' expect_true(is_tree_prior_name("coalescent_constant_population"))
#' expect_true(is_tree_prior_name("coalescent_exp_population"))
#' expect_true(is_tree_prior_name("yule"))
#' @author Richèl J.C. Bilderbeek
#' @export
is_tree_prior_name <- function(name) {
  name %in% beautier::get_tree_prior_names()
}
