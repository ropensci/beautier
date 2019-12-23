#' Creates the XML of an MRCA prior,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_prior_to_xml_state <- function(
  mrca_prior,
  has_non_strict_clock_model = FALSE
) {
  testit::assert(beautier::is_mrca_prior(mrca_prior))
  if (mrca_prior$is_monophyletic == FALSE &&
      beautier::is_one_na(mrca_prior$mrca_distr)) return(NULL)
  if (mrca_prior$is_monophyletic == TRUE &&
    beautier::is_one_na(mrca_prior$mrca_distr)) return(NULL)
  if (!has_non_strict_clock_model) {
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    paste0(
      "<parameter ",
      "id=\"clockRate.c:", mrca_prior$alignment_id, "\" ",
      "name=\"stateNode\">1.0</parameter>"
    )
  }
}
