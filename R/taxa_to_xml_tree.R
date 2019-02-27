#' Creates the \code{tree} section (part of the \code{state} section)
#' of a phylogeny and/or taxa
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
taxa_to_xml_tree <- function(
  id,
  tipdates_filename = NA
) {
  testit::assert(is_id(id)) # nolint beautier function
  if (is_one_na(tipdates_filename)) { # nolint beautier function
    no_taxa_to_xml_tree(id = id) # nolint beautier function
  } else {
    tipdate_taxa_to_xml_tree( # nolint beautier function
      id = id,
      tipdates_filename = tipdates_filename
    )
  }
}

#' Creates the \code{tree} section
#' (part of the \code{state} section)
#' when there is no tip-dating
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
no_taxa_to_xml_tree <- function(
  id
) {
  testit::assert(is_id(id)) # nolint beautier function
  text <- NULL
  text <- c(text, paste0("<tree id=\"Tree.t:", id, "\" name=\"stateNode\">"))
  text <- c(text, paste0("    <taxonset id=\"TaxonSet.", id, "\" ",
                         "spec=\"TaxonSet\">"))
  text <- c(text, paste0("        <alignment idref=\"", id, "\"/>")) # nolint this is no absolute path
  text <- c(text, "    </taxonset>")
  text <- c(text, "</tree>")
  text
}


#' Creates the \code{tree} section
#' (part of the \code{state} section)
#' when there is tip-dating
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
tipdate_taxa_to_xml_tree <- function(
  id,
  tipdates_filename
) {
  testit::assert(is_id(id)) # nolint beautier function
  testit::assert(!is_one_na(tipdates_filename)) # nolint beautier function
  trait_set_str <- create_trait_set_string(
    utils::read.table(tipdates_filename, sep = "\t")
  )
  c(
    paste0("<tree id=\"Tree.t:", id, "\" name=\"stateNode\">"),
    paste0("    <trait id=\"dateTrait.t:", id, "\" spec=\"beast.evolution.tree.TraitSet\" traitname=\"date-forward\" value=\"", trait_set_str, "\">"), # nolint indeed a long line
    paste0("        <taxa id=\"TaxonSet.", id, "\" spec=\"TaxonSet\">"),
    paste0("            <alignment idref=\"", id, "\"/>"), # nolint this is no absolute path
    "        </taxa>",
    "    </trait>",
    "    <taxonset idref=\"TaxonSet.G_VII_pre2003_msa\"/>", # nolint this is no absolute path
    "</tree>"
  )
}
