#' Get the number of distributions a tree prior has
#' @inheritParams default_params_doc
#' @return the number of distributions a tree prior has
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_equal(
#'   get_tree_priors_n_distrs(
#'     list(
#'       create_bd_tree_prior(), # has two distributions
#'       create_ccp_tree_prior() # has one distribution
#'     )
#'   ),
#'   3
#' )
#' @export
get_tree_priors_n_distrs <- function(
  tree_priors
) {
  if (!beautier::are_tree_priors(tree_priors)) {
    stop("'tree_priors' must be a list of one or more tree priors")
  }
  n <- 0
  for (tree_prior in tree_priors) {
    testit::assert(beautier::is_tree_prior(tree_prior))
    n <- n + beautier::get_tree_prior_n_distrs(tree_prior)
  }
  n
}
