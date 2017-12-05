#' Converts one or more clock models to the \code{state} section of the
#' XML as text
#' @param clock_models a list of one or more clock_models,
#'   as created by \code{\link{create_clock_model}}
#' @return lines of XML text, without indentation nor \code{state}
#'   tags
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_state <- function(
  clock_models
) {
  testit::assert(beautier::are_clock_models(clock_models))

  # Remove the clock models that share a same alignment
  clock_models <- get_unlinked_clock_models(clock_models)

  text <- NULL
  for (clock_model in clock_models) {
    text <- c(text,
      clock_model_to_xml_state(clock_model)
    )
  }

  # Remove the first line of the first clock model, if any
  clock_model <- clock_models[[1]]
  line_to_remove <- NULL
  if (is_strict_clock_model(clock_model)) {
    line_to_remove <- clock_model_to_xml_state(clock_model)
  } else {
    # Will fail for unimplemented clock models
    testit::assert(is_rln_clock_model(clock_model))
    # clock_model_to_xml_state returns three lines, only use the first
    line_to_remove <- clock_model_to_xml_state(clock_model)[1]
  }
  testit::assert(!is.null(line_to_remove))
  text <- text[ text != line_to_remove ]


  text
}
