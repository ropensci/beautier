#' Internal function
#'
#' Converts a clock model to the \code{operators} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
clock_model_to_xml_operators <- function(
  inference_model
) {
  if (beautier::is_strict_clock_model(inference_model$clock_model)) {
    return(strict_clock_model_to_xml_operators(inference_model))
  } else {
    # Will fail on unimplemented clock models
    testthat::expect_true(
      beautier::is_rln_clock_model(inference_model$clock_model)
    )
    return(rln_clock_model_to_xml_operators(inference_model))
  }
}
