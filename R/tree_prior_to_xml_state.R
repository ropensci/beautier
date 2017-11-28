#' Creates the XML of a tree prior,
#'   as used in the \code{state} section
#' @param tree_prior a tree prior,
#'   as returned by \code{\link{create_tree_prior}})
#' @return the tree prior as XML text
#' @author Richel J.C. Bilderbeek
tree_prior_to_xml_state <- function(
  tree_prior
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  id <- tree_prior$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  if (is_yule_tree_prior(tree_prior)) {
    text <- c(text, paste0("<parameter ",
      "id=\"birthRate.t:", id, "\" ",
      "name=\"stateNode\">1.0</parameter>"))
  }
  text
}

