#' Creates the XML of a phylogeny,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @param id the ID of the alignment
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
phylo_to_xml_state <- function(
  id = "irrelevant",
  inference_model = "irrelevant",
  tipdates_filename = "deprecated"
) {
  stop("Use 'taxa_to_xml_tree'")
}
