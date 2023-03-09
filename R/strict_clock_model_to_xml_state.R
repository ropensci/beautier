#' Internal function
#'
#' Converts a strict clock model to the `state` section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
strict_clock_model_to_xml_state <- function(
  inference_model
) {
  beautier::check_inference_model(inference_model)
  testthat::expect_true(beautier::has_strict_clock_model(inference_model))

  # Don't be smart yet
  clock_model <- inference_model$clock_model

  text <- NULL

  # return("<parameter id=\"clockRate.c:anthus_aco_sub\" spec=\"parameter.RealParameter\" lower=\"0.00277\" name=\"stateNode\" upper=\"0.00542\">0.0035</parameter>")

  if (!beautier::has_tip_dating(inference_model)) {
    return(text)
  }

  text <- c(
    text,
    beautier::create_clock_rate_state_node_parameter_xml(inference_model)
  )
  text
}
