#' Internal function to create the XML of an MRCA prior,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' created <- mrca_prior_to_xml_state(
#'   inference_model = create_inference_model(
#'     mrca_prior = create_mrca_prior(
#'       alignment_id = "test_output_0",
#'       mrca_distr = create_normal_distr(id = 42)
#'     ),
#'     clock_model = create_strict_clock_model()
#'   )
#' )
#' expect_match(created, "<parameter id=\"clockRate.c:")
#' @export
mrca_prior_to_xml_state <- function(
  inference_model,
  mrca_prior = "deprecated",
  has_non_strict_clock_model = "deprecated"
) {
  if (mrca_prior != "deprecated") {
    stop("'mrca_prior' is deprecated, use 'inference_model' instead")
  }
  if (has_non_strict_clock_model != "deprecated") {
    stop(
      "'has_non_strict_clock_model' is deprecated, ",
      "use 'inference_model' instead"
    )
  }

  # Do not be smart yet
  mrca_prior <- inference_model$mrca_prior
  has_non_strict_clock_model <- !beautier::is_strict_clock_model(
    inference_model$clock_model
  )
  if (beautier::is_one_na(mrca_prior)) {
    return(NULL)
  }
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
