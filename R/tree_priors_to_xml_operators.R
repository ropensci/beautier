#' Creates the XML of a list of one or more tree priors,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
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
