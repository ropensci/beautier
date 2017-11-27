#' Creates the XML of a random species tree,
#'   as used in the \code{state} section
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @return the random species tree as XML text
#' @author Richel J.C. Bilderbeek
#' @export
rnd_species_tree_to_xml_state <- function(
  id
) {
  testit::assert(beautier::is_id(id))
  text <- NULL
  text <- c(text, paste0("<tree id=\"Tree.t:",
    id, "\" name=\"stateNode\">"))
  text <- c(text, paste0("    <taxonset id=\"TaxonSet.",
    id, "\" spec=\"TaxonSet\">"))
  text <- c(text, paste0("        <alignment idref=\"",
    id, "\"/>"))
  text <- c(text, "    </taxonset>")
  text <- c(text, "</tree>")
  text
}
