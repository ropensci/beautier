#' Creates the XML of a random phylogeny,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richel J.C. Bilderbeek
rnd_phylo_to_xml_state <- function(
  id
) {
  testit::assert(is_id(id))
  text <- NULL
  text <- c(text, paste0("<tree id=\"Tree.t:", id, "\" name=\"stateNode\">"))
  text <- c(text, paste0("    <taxonset id=\"TaxonSet.", id, "\" ",
    "spec=\"TaxonSet\">"))
  text <- c(text, paste0("        <alignment idref=\"", id, "\"/>"))
  text <- c(text, "    </taxonset>")
  text <- c(text, "</tree>")
  text
}
