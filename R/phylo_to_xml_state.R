#' Creates the XML of a phylogeny,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @param id the ID of the alignment
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
phylo_to_xml_state <- function(
  id,
  inference_model,
  tipdates_filename = "deprecated"
) {
  if (tipdates_filename != "deprecated") {
    stop("'tipdates_filename' is deprecated, use 'inference_model' instead")
  }
  testit::assert(beautier::is_id(id))
  beautier::taxa_to_xml_tree(
    id = id,
    inference_model = inference_model
  )
}
