#' Get the number of parameters a list of tree priors has
#' @inheritParams default_params_doc
#' @return the number of parameters the tree priors have
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # Two
#' get_tree_priors_n_params(
#'   list(
#'     create_bd_tree_prior(), # zero
#'     create_cep_tree_prior() # two
#'   )
#' )
#'
#' check_empty_beautier_folder()
#' @export
get_tree_priors_n_params <- function(
  tree_priors
) {
  if (!beautier::are_tree_priors(tree_priors)) {
    stop("'tree_priors' must be a list of tree priors")
  }
  n <- 0
  for (tree_prior in tree_priors) {
    n <- n + beautier::get_tree_prior_n_params(tree_prior)
  }
  n
}
