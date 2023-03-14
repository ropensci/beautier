#' Internal function
#'
#' Converts a strict clock model to the `state` section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
strict_clock_model_to_xml_state <- function( # nolint indeed a long internal function name
  inference_model
) {
  beautier::check_inference_model(inference_model)
  testthat::expect_true(beautier::has_strict_clock_model(inference_model))

  # Don't be smart yet
  clock_model <- inference_model$clock_model

  text <- NULL

  if (inference_model$clock_model$clock_rate_param$estimate) {
    text <- c(
      text,
      paste0(
        "<parameter ",
        "id=\"clockRate.c:", inference_model$clock_model$id, "\" ",
        "spec=\"parameter.RealParameter\" ",
        "lower=\"", inference_model$clock_model$clock_rate_distr$lower, "\" ",
        "name=\"stateNode\" ",
        "upper=\"", inference_model$clock_model$clock_rate_distr$upper,"\">",
        inference_model$clock_model$clock_rate_param$value,
        "</parameter>"
      )
    )

  }

  if (!beautier::has_tip_dating(inference_model)) {
    return(text)
  }

  text <- c(
    text,
    beautier::create_clock_rate_state_node_parameter_xml(inference_model)
  )
  text
}
