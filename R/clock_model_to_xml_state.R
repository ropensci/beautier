#' Internal function
#'
#' Converts a clock model to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
clock_model_to_xml_state <- function(
  inference_model
) {
  check_inference_model(inference_model)

  if (has_strict_clock_model(inference_model)) {
    return(strict_clock_model_to_xml_state(inference_model))
  } else {
    check_true(has_rln_clock_model(inference_model))
    return(rln_clock_model_to_xml_state(inference_model))
  }
}
