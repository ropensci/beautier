#' Creates the XML of a list of one or more tree priors,
#'   as used in the \code{operators} section
#' @inheritParams default_params_doc
#' @return the tree priors as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
tree_priors_to_xml_operators <- function(
  tree_priors,
  fixed_crown_ages = rep(FALSE, times = length(tree_priors))
) {

  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(is.logical(fixed_crown_ages))
  testit::assert(length(tree_priors) == length(fixed_crown_ages))

  text <- NULL
  for (i in seq_along(tree_priors)) {

    tree_prior <- tree_priors[[i]]
    fixed_crown_age <- fixed_crown_ages[i]

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
