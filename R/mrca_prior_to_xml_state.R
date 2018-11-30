#' Creates the XML of an MRCA prior,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richel J.C. Bilderbeek
#' @noRd
mrca_prior_to_xml_state <- function(
  mrca_prior,
  has_non_strict_clock_model = FALSE
) {
  testit::assert(is_mrca_prior(mrca_prior)) # nolint internal function
  if (mrca_prior$is_monophyletic == FALSE &&
      is.na(mrca_prior$mrca_distr)) return(NULL)
  if (mrca_prior$is_monophyletic == TRUE &&
    is.na(mrca_prior$mrca_distr)) return(NULL)
  if (!has_non_strict_clock_model) {
    paste0(
      "<parameter ",
      "id=\"clockRate.c:", mrca_prior$alignment_id, "\" ",
      "name=\"stateNode\">1.0</parameter>"
    )
  }
}
