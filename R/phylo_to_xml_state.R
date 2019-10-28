#' Creates the XML of a phylogeny,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @param id the ID of the alignment
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
phylo_to_xml_state <- function(
  id,
  tipdates_filename = NA
) {
  testit::assert(beautier::is_id(id))
  taxa_to_xml_tree( # nolint beautier function
    id = id,
    tipdates_filename = tipdates_filename
  )
}
