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
  check_inference_model(inference_model)
  check_true(has_strict_clock_model(inference_model))

  text <- NULL

  if (inference_model$clock_model$clock_rate_param$estimate) {
    param_xml <- paste0(
      "<parameter ",
      "id=\"clockRate.c:", inference_model$clock_model$id, "\" ",
      "spec=\"parameter.RealParameter\" "
    )
    lower <- inference_model$clock_model$clock_rate_distr$lower
    if (!is_one_na(lower)) {
      param_xml <- paste0(
        param_xml,
        "lower=\"", inference_model$clock_model$clock_rate_distr$lower, "\" "
      )
    }
    param_xml <- paste0(
      param_xml,
      "name=\"stateNode\""
    )
    upper <- inference_model$clock_model$clock_rate_distr$upper
    if (!is_one_na(upper)) {
      param_xml <- paste0(
        param_xml,
        " upper=\"", inference_model$clock_model$clock_rate_distr$upper, "\""
      )
    }
    param_xml <- paste0(
      param_xml,
      ">",
      inference_model$clock_model$clock_rate_param$value,
      "</parameter>"
    )
    text <- c(text, param_xml)
  }

  if (!has_tip_dating(inference_model)) {
    return(text)
  }

  text <- c(
    text,
    create_clock_rate_state_node_parameter_xml(inference_model)
  )
  text
}
