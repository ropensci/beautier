#' Converts one or more clock models to the \code{state} section of the
#' XML as text
#' @inheritParams default_params_doc
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_state <- function(
  clock_models
) {
  testit::assert(are_clock_models(clock_models))

  # Remove the clock models that share a same alignment
  clock_models <- get_unlinked_clock_models(clock_models) # nolint internal function
  testit::assert(are_clock_models(clock_models))

  text <- NULL
  for (clock_model in clock_models) {
    text <- c(text,
      clock_model_to_xml_state(clock_model)
    )
  }

  # Remove the first line of the first clock model, if any
  # if no MRCA prior is used
  clock_model <- clock_models[[1]]
  line_to_remove <- clock_model_to_xml_state(clock_model) # nolint
  if (is_rln_clock_model(clock_model)) {
    # A RLN clock model returns three lines, only remove the first
    testit::assert(length(line_to_remove) == 3)
    line_to_remove <- line_to_remove[1]
  }
  testit::assert(!is.null(line_to_remove))
  text <- text[text != line_to_remove]
  text
}
