#' Creates the XML of a phylogeny,
#'   as used in the \code{state} section
#' @param phylo the phylogeny. If NA, a random phylogeny is used
#' @param id the ID of the alignment
#' @return the random phylogeny as XML text
#' @author Richel J.C. Bilderbeek
#' @noRd
phylo_to_xml_state <- function(
  phylo,
  id
) {
  testit::assert(is_id(id)) # nolint internal function
  if (!is_phylo(phylo)) { # nolint internal function
    return(taxa_to_xml_tree(id)) # nolint internal function
  }
  testit::assert(is_phylo(phylo)) # nolint internal function
  text <- NULL
  text <- c(text, paste0("<stateNode spec=\"beast.util.TreeParser\" ",
    "id=\"Tree.t:", id, "\" IsLabelledNewick=\"true\" ",
    "adjustTipHeights=\"false\" taxa=\"@", id, "\" ",
    "newick=\"", ape::write.tree(phylo), "\">"))
  text <- c(text, paste0("</stateNode>"))
  text
}
