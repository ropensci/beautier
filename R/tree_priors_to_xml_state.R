#' Converts one or more tree priors to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richel J.C. Bilderbeek
tree_priors_to_xml_state <- function(
  tree_priors
) {
  testit::assert(are_tree_priors(tree_priors))

  # Remove the tree priors that share a same alignment
  tree_priors <- get_unlinked_tree_priors(tree_priors) # nolint internal function
  testit::assert(are_tree_priors(tree_priors))

  text <- NULL
  for (tree_prior in tree_priors) {
    text <- c(text,
      tree_prior_to_xml_state(tree_prior)
    )
  }

  text
}
