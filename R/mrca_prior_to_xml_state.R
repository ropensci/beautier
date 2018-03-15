#' Creates the XML of an MRCA prior,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @param is_first is this the first MRCA prior?
#' @return the tree prior as XML text
#' @author Richel J.C. Bilderbeek
mrca_prior_to_xml_state <- function(
  mrca_prior,
  has_non_strict_clock_model = FALSE,
  is_first = TRUE
) {
  testit::assert(is_mrca_prior(mrca_prior))
  if (mrca_prior$is_monophyletic == FALSE) return(NULL)
  if (is_first == FALSE) return(NULL)
  if (!has_non_strict_clock_model) {
    paste0(
      "<parameter ",
      "id=\"clockRate.c:", mrca_prior$alignment_id, "\" ",
      "name=\"stateNode\">1.0</parameter>"
    )
  }
}
