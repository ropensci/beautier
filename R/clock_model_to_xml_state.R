#' Converts a clock model to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richèl J.C. Bilderbeek
#' @export
clock_model_to_xml_state <- function(
  inference_model,
  clock_model = "deprecated",
  has_tip_dating = "deprecated"
) {
  testthat::expect_equal(clock_model, "deprecated")
  testthat::expect_equal(has_tip_dating, "deprecated")
  # Don't be smart yet
  clock_model <- inference_model$clock_model
  has_tip_dating <- !beautier::is_one_na(inference_model$tipdates_filename)
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(clock_model$id))

  text <- NULL
  if (beautier::is_strict_clock_model(clock_model) || has_tip_dating == TRUE) {
    parameter_xml <- paste0(
      "<parameter id=\"clockRate.c:", clock_model$id, "\" "
    )
    if (inference_model$beauti_options$beast2_version == "2.6") {
      parameter_xml <- paste0(
        parameter_xml,
        "spec=\"parameter.RealParameter\" "
      )
    }
    parameter_xml <- paste0(
      parameter_xml,
      "name=\"stateNode\">", clock_model$clock_rate_param$value,
      "</parameter>"
    )
    text <- c(text, parameter_xml)
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
