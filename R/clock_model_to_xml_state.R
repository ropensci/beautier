#' Converts a clock model to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
clock_model_to_xml_state <- function(
  clock_model,
  has_tip_dating = FALSE
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(clock_model$id))

  text <- NULL
  if (beautier::is_strict_clock_model(clock_model) || has_tip_dating == TRUE) {
    text <- c(
      text,
      paste0("<parameter id=\"clockRate.c:", clock_model$id, "\" ",
        "name=\"stateNode\">", clock_model$clock_rate_param$value,
        "</parameter>"
      )
    )
  } else {
    # Fails on unimplemented clock models
    testit::assert(beautier::is_rln_clock_model(clock_model))
    testit::assert(!beautier::is_one_na(clock_model$mean_clock_rate))
    testit::assert(!beautier::is_one_na(clock_model$dimension))

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
