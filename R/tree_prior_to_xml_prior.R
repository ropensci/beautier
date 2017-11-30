#' Creates the XML of a tree prior,
#'   as used in the \code{prior} section
#'   of the \code{distribution}
#' @param tree_prior a tree prior,
#'   as returned by \code{\link{create_tree_prior}})
#' @return the tree prior as XML text
#' @author Richel J.C. Bilderbeek
tree_prior_to_xml_prior <- function(
  tree_prior
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  text <- NULL
  if (beautier::is_bd_tree_prior(tree_prior)) {
    text <- c(text, bd_tree_prior_to_xml_prior(tree_prior)) # nolint internal function
  } else if (beautier::is_cbs_tree_prior(tree_prior)) {
    # Nothing to do
  } else if (beautier::is_ccp_tree_prior(tree_prior)) {
    text <- c(text, ccp_tree_prior_to_xml_prior(tree_prior)) # nolint internal function
  } else if (beautier::is_cep_tree_prior(tree_prior)) {
    text <- c(text, cep_tree_prior_to_xml_prior(tree_prior)) # nolint internal function
  } else {
    testit::assert(beautier::is_yule_tree_prior(tree_prior))
    text <- c(text, yule_tree_prior_to_xml_prior(tree_prior)) # nolint internal function
  }
  text
}
