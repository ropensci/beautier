#' Creates the XML of an MRCA prior,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richel J.C. Bilderbeek
mrca_prior_to_xml_state <- function(
  mrca_prior
) {
  testit::assert(is_mrca_prior(mrca_prior))
  if (mrca_prior$is_monophyletic == FALSE) return(NULL)
  paste0(
    "<parameter ",
    "id=\"clockRate.c:", mrca_prior$alignment_id, "\" ",
    "name=\"stateNode\">1.0</parameter>"
  )
}
