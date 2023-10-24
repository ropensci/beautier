#' Internal function
#'
#' Creates the clock model's XML for the tracelog section
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @examples
#' check_empty_beautier_folder()
#'
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
clock_model_to_xml_tracelog <- function(
  inference_model
) {
  if (is_strict_clock_model(inference_model$clock_model)) {
    return(strict_clock_model_to_xml_tracelog(inference_model))
  } else {
    # Will fail on unimplemented clock models
    check_true(
      is_rln_clock_model(inference_model$clock_model)
    )
    return(rln_clock_model_to_xml_tracelog(inference_model))
  }
}
