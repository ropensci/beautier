#' Creates the \code{taxonset} section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' @inheritParams default_params_doc
#' @param taxa_names_with_ids taxa names that already have received
#'   an ID. Causes the XML to \code{idref} these
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   # <taxonset id="all" spec="TaxonSet">
#'   #     <taxon id="626029_aco" spec="Taxon"/>
#'   #     <taxon id="630116_aco" spec="Taxon"/>
#'   #     <taxon id="630210_aco" spec="Taxon"/>
#'   #     <taxon id="B25702_aco" spec="Taxon"/>
#'   #     <taxon id="61430_aco" spec="Taxon"/>
#'   # </taxonset>
#' @noRd
mrca_prior_to_xml_taxonset <- function(
  mrca_prior,
  taxa_names_with_ids = NULL
) {
  testit::assert(is_mrca_prior(mrca_prior)) # nolint beautier function
  text <- NULL
  testit::assert(!is_one_na(mrca_prior$taxa_names)) # nolint beautier function
  for (taxon_name in mrca_prior$taxa_names) {
    text <- c(text, paste0("<taxon id=\"", taxon_name, "\" spec=\"Taxon\"/>")) # nolint this is no absolute path
  }
  text <- indent(text, n_spaces = 4) # nolint beautier function
  text <- c(
    paste0("<taxonset id=\"", mrca_prior$name, "\" spec=\"TaxonSet\">"),
    text
  )
  text <- c(text, "</taxonset>")
  text
}
