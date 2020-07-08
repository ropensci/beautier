#' Creates the XML of an MRCA prior,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#' created <- mrca_prior_to_xml_state(
#'  mrca_prior = create_mrca_prior(
#'     alignment_id = "test_output_0",
#'     mrca_distr = create_normal_distr(id = 42)
#'   ),
#'   has_non_strict_clock_model = FALSE
#' )
#' expect_match(created, "<parameter id=\"clockRate.c:")
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
