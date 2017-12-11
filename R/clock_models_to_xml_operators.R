#' Create all clock models' operators' XML text
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_operators <- function(
  clock_models
) {
  testit::assert(beautier::are_clock_models(clock_models))
  text <- NULL
  for (clock_model in clock_models) {
    text <- c(
      text,
      clock_model_to_xml_operators(clock_model)
    )
  }
  text
}
