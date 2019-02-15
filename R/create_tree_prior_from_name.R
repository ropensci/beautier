#' Create a tree prior from name
#' @inheritParams default_params_doc
#' @return a tree prior
#' @seealso Use \link{create_tree_prior} to create a tree prior
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   tree_prior_names <- get_tree_prior_names()
#'   for (tree_prior_name in tree_prior_names) {
#'     tree_prior <- create_tree_prior_from_name(tree_prior_name)
#'     testthat::expect_equal(tree_prior_name, tree_prior$name)
#'   }
#' @export
create_tree_prior_from_name <- function(tree_prior_name) {
  if (tree_prior_name == "yule") {
    beautier::create_yule_tree_prior()
  } else if (tree_prior_name == "birth_death") {
    beautier::create_bd_tree_prior()
  } else if (tree_prior_name == "coalescent_bayesian_skyline") {
    beautier::create_cbs_tree_prior()
  } else if (tree_prior_name == "coalescent_constant_population") {
    beautier::create_ccp_tree_prior()
  } else {
    testit::assert(tree_prior_name == "coalescent_exp_population")
    beautier::create_cep_tree_prior()
  }
}
