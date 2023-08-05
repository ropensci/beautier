#' Internal function
#'
#' Converts a clock model to the \code{operators} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @export
strict_clock_model_to_xml_operators <- function( # nolint indeed a long internal function name
  inference_model
) {
  # Don't be smart yet
  clock_model <- inference_model$clock_model

  testthat::expect_true(beautier::is_strict_clock_model(clock_model))
  id <- clock_model$id

  # May not need ID at all, if it is the first and strict clock model
  text <- NULL

  if (inference_model$clock_model$clock_rate_param$estimate) {
    text <- c(
      text,
      paste0(
        "<operator id=\"StrictClockRateScaler.c:", id, "\" ",
        "spec=\"ScaleOperator\" ",
        "parameter=\"@clockRate.c:", id, "\" ",
        "weight=\"3.0\"/>"
      ),
      paste0(
        "<operator id=\"strictClockUpDownOperator.c:", id, "\" ",
        "spec=\"UpDownOperator\" ",
        "scaleFactor=\"0.75\" ",
        "weight=\"3.0\">"
      ),
      beautier::indent(paste0("<up idref=\"clockRate.c:", id, "\"/>")),
      beautier::indent(paste0("<down idref=\"Tree.t:", id, "\"/>")),
      "</operator>"
    )
  }

  if (beautier::has_mrca_prior_with_distr(inference_model) ||
      beautier::has_tip_dating(inference_model)
  ) {
    text <- c(
      text,
      beautier::create_strict_clock_rate_scaler_operator_xml(inference_model)
    )
    text <- c(
      text,
      paste0(
        "<operator id=\"strictClockUpDownOperator.c:", id, "\" ",
        "spec=\"UpDownOperator\" scaleFactor=\"0.75\" weight=\"3.0\">"
      )
    )
    text <- c(text, paste0("    <up idref=\"clockRate.c:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("    <down idref=\"Tree.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("</operator>"))
  }
  text
}
