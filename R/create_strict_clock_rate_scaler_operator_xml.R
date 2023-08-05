#' Internal function
#'
#' Creates the \code{StrictClockRateScaler} operator
#' such as:
#' \code{
#'   ...
#' }
#' @inheritParams default_params_doc
#' @return the following XML:
#' \code{
#'   ...
#' }
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' create_strict_clock_rate_scaler_operator_xml(
#'   create_inference_model(
#'     clock_model = create_strict_clock_model(id = 314)
#'   )
#' )
#'
#' check_empty_beautier_folder()
#' @export
create_strict_clock_rate_scaler_operator_xml <- function(inference_model) { # nolint indeed a long function name
  beautier::check_inference_model(inference_model)
  clock_model <- inference_model$clock_model
  testthat::expect_true(beautier::is_strict_clock_model(clock_model))
  id <- clock_model$id
  testthat::expect_true(beautier::is_id(id))
  xml <- paste0(
    "<operator id=\"StrictClockRateScaler.c:", id, "\" ",
    "spec=\"ScaleOperator\" parameter=\"@clockRate.c:", id, "\" "
  )
  if (clock_model$rate_scaler_factor != "") {
    xml <- paste0(xml, "scaleFactor=\"", clock_model$rate_scaler_factor, "\" ")
  }
  if (1 == 2) {
    if (inference_model$beauti_options$beast2_version != "2.6") {
      xml <- paste0(xml, "scaleFactor=\"0.75\" ")
    }
  }
  xml <- paste0(xml, "weight=\"3.0\"/>")
  xml
}
