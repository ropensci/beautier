#' Converts a clock model to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @noRd
clock_model_to_xml_state <- function(
  clock_model,
  has_tip_dating = FALSE
) {
  testit::assert(is_clock_model(clock_model)) # nolint beautier function
  id <- clock_model$id
  testit::assert(is_id(clock_model$id)) # nolint beautier function

  text <- NULL
  if (is_strict_clock_model(clock_model) || has_tip_dating == TRUE) { # nolint beautier function
    text <- c(
      text,
      paste0("<parameter id=\"clockRate.c:", clock_model$id, "\" ",
        "name=\"stateNode\">", clock_model$clock_rate_param$value,
        "</parameter>"
      )
    )
  } else {
    # Fails on unimplemented clock models
    testit::assert(is_rln_clock_model(clock_model)) # nolint beautier function

    testit::assert(!is_one_na(clock_model$mean_clock_rate)) # nolint beautier function
    testit::assert(!is_one_na(clock_model$dimension)) # nolint beautier function

    text <- c(text, paste0("<parameter id=\"ucldMean.c:", id, "\" ",
        "name=\"stateNode\">", clock_model$mean_clock_rate, "</parameter>")
    )
    # ucldStdev.c is always 0.1, cannot set it to other value
    text <- c(text, paste0("<parameter id=\"ucldStdev.c:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">0.1</parameter>"))
    # value is always 1, dimension d = 2n - 2, where n is the number of taxa
    text <- c(text, paste0("<stateNode id=\"rateCategories.c:", id, "\" ",
      "spec=\"parameter.IntegerParameter\" ",
      "dimension=\"", clock_model$dimension, "\">1</stateNode>"))
  }

  text
}
