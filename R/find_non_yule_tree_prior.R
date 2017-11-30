#' Finds the first non-Yule tree prior from a list of one or more tree priors
#' @param tree_priors a list of one or more tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @return the first non-Yule tree prior, NULL if such a tree prior is
#'   absent
#' @author Richel J.C. Bilderbeek
#' @export
find_non_yule_tree_prior <- function(
  tree_priors
) {
  if (!beautier::are_tree_priors(tree_priors)) {
    stop("'tree_priors' must be a list of tree priors")
  }
  for (tree_prior in tree_priors) {
    if (!is_yule_tree_prior(tree_prior)) {
      return(tree_prior)
    }
  }
  NULL
}
