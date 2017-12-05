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

  text <- NULL
  for (clock_model in clock_models) {
    text <- c(
      text,
      clock_model_to_xml_state(
        clock_model
      )
    )
  }
  text
}
