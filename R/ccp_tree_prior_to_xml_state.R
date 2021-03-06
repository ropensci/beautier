#' Convert a CCP tree prior to the XML as part of the \code{state} section
ccp_tree_prior_to_xml_state <- function(tree_prior) {
  testit::assert(beautier::is_id(tree_prior$id))
  paste0("<parameter id=\"popSize.t:", tree_prior$id, "\" ",
      "name=\"stateNode\">0.3</parameter>"
  )
}
