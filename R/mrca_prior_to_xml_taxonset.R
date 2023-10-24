#' Creates the \code{taxonset} section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#'
#' Creates the \code{taxonset} section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#'
#' \code{
#'   <taxonset id="all" spec="TaxonSet">
#'       <taxon id="626029_aco" spec="Taxon"/>
#'       <taxon id="630116_aco" spec="Taxon"/>
#'       <taxon id="630210_aco" spec="Taxon"/>
#'       <taxon id="B25702_aco" spec="Taxon"/>
#'       <taxon id="61430_aco" spec="Taxon"/>
#'   </taxonset>
#' }
#' @inheritParams default_params_doc
#' @param taxa_names_with_ids taxa names that already have received
#'   an ID. Causes the XML to \code{idref} these
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_prior_to_xml_taxonset <- function(
  mrca_prior,
  taxa_names_with_ids = NULL
) {
  check_true(is_mrca_prior(mrca_prior))
  text <- NULL
  check_true(!is_one_na(mrca_prior$taxa_names))
  for (taxon_name in mrca_prior$taxa_names) {
    text <- c(text, paste0("<taxon id=\"", taxon_name, "\" spec=\"Taxon\"/>")) # nolint this is no absolute path
  }
  text <- indent(text)
  text <- c(
    paste0("<taxonset id=\"", mrca_prior$name, "\" spec=\"TaxonSet\">"),
    text
  )
  text <- c(text, "</taxonset>")
  text
}
