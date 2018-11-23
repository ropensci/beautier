#' Creates the \code{tree} section (part of the \code{state} section)
#' of a phylogeny and/or taxa
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richel J.C. Bilderbeek
#' @noRd
taxa_to_xml_tree <- function(
  id,
  tipdates_filename = NA
) {
  testit::assert(is_id(id)) # nolint internal function
  text <- NULL
  text <- c(text, paste0("<tree id=\"Tree.t:", id, "\" name=\"stateNode\">"))
  text <- c(text, paste0("    <taxonset id=\"TaxonSet.", id, "\" ",
    "spec=\"TaxonSet\">"))
  text <- c(text, paste0("        <alignment idref=\"", id, "\"/>"))
  text <- c(text, "    </taxonset>")
  text <- c(text, "</tree>")
  text
}
