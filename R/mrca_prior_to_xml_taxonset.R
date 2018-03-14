#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @param taxa_names_with_ids taxa names that already have received
#'   an ID. Causes the XML to \code{idref} these
#' @author Richel J.C. Bilderbeek
#' @examples
#'   # <taxonset id="all" spec="TaxonSet">
#'   #     <taxon id="626029_aco" spec="Taxon"/>
#'   #     <taxon id="630116_aco" spec="Taxon"/>
#'   #     <taxon id="630210_aco" spec="Taxon"/>
#'   #     <taxon id="B25702_aco" spec="Taxon"/>
#'   #     <taxon id="61430_aco" spec="Taxon"/>
#'   # </taxonset>
mrca_prior_to_xml_taxonset <- function(
  mrca_prior,
  taxa_names_with_ids = NULL
) {
  testit::assert(is_mrca_prior(mrca_prior))
  text <- NULL
  for (taxon_name in mrca_prior$taxa_names) {
    if (taxon_name %in% taxa_names_with_ids) {
      text <- c(text, paste0("<taxon idref=\"", taxon_name, "\"/>"))
    } else {
      text <- c(text, paste0("<taxon id=\"", taxon_name, "\" spec=\"Taxon\"/>"))
    }
  }
  text <- indent(text, n_spaces = 4)
  text <- c(
    paste0("<taxonset id=\"", mrca_prior$name, "\" spec=\"TaxonSet\">"),
    text
  )
  text <- c(text, "</taxonset>")
  text
}
