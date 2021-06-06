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
  text
}
