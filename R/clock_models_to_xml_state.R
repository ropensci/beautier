#' Internal function
#'
#' Converts one or more clock models to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
clock_models_to_xml_state <- function(
  inference_model
) {
  if (beautier::has_strict_clock_model(inference_model) &&
    !beautier::has_tip_dating(inference_model)
  ) {
    return(NULL)
  }

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
  text
}
