#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @author Richel J.C. Bilderbeek
tree_priors_to_xml_prior_distr <- function( # nolint internal function
  tree_priors
) {
  testit::assert(are_tree_priors(tree_priors))

  text <- NULL
  for (tree_prior in tree_priors) {
    text <- c(text, tree_prior_to_xml_prior_distr(tree_prior)) # nolint internal function
  }
  text
}
