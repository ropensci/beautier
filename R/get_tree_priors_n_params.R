#' Get the number of parameters a list of tree priors has
#' @inheritParams default_params_doc
#' @return the number of parameters the tree priors have
#' @author Richel J.C. Bilderbeek
#' @examples
#'  testit::assert(
#'    beautier:::get_tree_priors_n_params(
#'      list(
#'        create_bd_tree_prior(), # zero
#'        create_cep_tree_prior() # two
#'      )
#'    ) == 2
#'  )
get_tree_priors_n_params <- function(
  tree_priors
) {
  if (!are_tree_priors(tree_priors)) {
    stop("'tree_priors' must be a list of tree priors")
  }
  n <- 0
  for (tree_prior in tree_priors) {
    n <- n + get_tree_prior_n_params(tree_prior)
  }
  n
}
