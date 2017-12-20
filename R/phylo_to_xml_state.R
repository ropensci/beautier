#' Creates the XML of a phylogeny,
#'   as used in the \code{state} section
#' @param phylo the phylogeny. If NA, a random phylogeny is used
#' @param id the ID of the alignment
#' @return the random phylogeny as XML text
#' @author Richel J.C. Bilderbeek
phylo_to_xml_state <- function(
  phylo,
  id
) {
  testit::assert(is_id(id))
  if (!is_phylo(phylo)) {
    return(rnd_phylo_to_xml_state(id)) # nolint internal function
  }
  testit::assert(is_phylo(phylo))
  text <- NULL
  text <- c(text, paste0("<stateNode spec=\"beast.util.TreeParser\" ",
    "id=\"Tree.t:", id, "\" IsLabelledNewick=\"true\" ",
    "adjustTipHeights=\"false\" taxa=\"@", id, "\" ",
    "newick=\"", ape::write.tree(phylo), "\">"))
  text <- c(text, paste0("</stateNode>"))
  text
}
