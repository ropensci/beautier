#' Creates the XML of a list of one or more tree priors,
#'   as used in the \code{operators} section
#' @param tree_priors a list of one or more tree priors,
#'   as returned by \code{\link{create_tree_prior}})
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @return the tree priors as XML text
#' @author Richel J.C. Bilderbeek
tree_priors_to_xml_operators <- function(
  tree_priors,
  fixed_crown_age
) {

  testit::assert(beautier::are_tree_priors(tree_priors))

  text <- NULL
  for (tree_prior in tree_priors) {
    text <- c(
      text,
      tree_prior_to_xml_operators(
        tree_prior = tree_prior,
        fixed_crown_age = fixed_crown_age
      )
    )
  }
  text
}
