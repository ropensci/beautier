#' Internal function
#'
#' Creates the XML of the \code{parameter} nodes of the \code{state} section
#' @inheritParams default_params_doc
#' @return NULL or lines of XML text,
#' all in the form \code{<parameter id=[...]>}
#' @author Richèl J.C. Bilderbeek
#' @export
clock_models_to_xml_state_parameters <- function( # nolint indeed a long function name
  inference_model
) {
  if (beautier::has_strict_clock_model(inference_model)) {
    if (beautier::has_tip_dating(inference_model)) {
      return("What to do here 1")
    } else {
      return(NULL)
    }
  } else {
    # Will stop here for unimplemented clock models
    testthat::expect_true(has_rln_clock_model(inference_model))
    if (beautier::has_tip_dating(inference_model)) {
      return("What to do here 2")
    } else {
      return("What to do here 3")
    }
  }

  if (1 == 2) {
    text <- beautier::clock_model_to_xml_state(
      inference_model = inference_model
    )
    # Remove the first line of the first clock model,
    # if no MRCA prior with a distribution is used
    if (beautier::has_rln_clock_model(inference_model) &&
        !beautier::is_mrca_prior_with_distr(inference_model$mrca_prior)) {
      # A RLN clock model returns three lines, only remove the first
      line_to_remove <- beautier::clock_model_to_xml_state(
        inference_model = inference_model
      )
      testit::assert(length(line_to_remove) == 3)
      text <- line_to_remove[
        stringr::str_remove_all(
          string = line_to_remove, pattern = ".*ucldMean\\.c:.*"
        ) != ""
      ]
    }
  }

  stop("never get here")
}
