#' Create all clock models' operators' XML text
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
clock_models_to_xml_operators <- function(
  clock_models
) {
  testit::assert(are_clock_models(clock_models))

  clock_models <- get_unlinked_clock_models(clock_models) # nolint internal function

  text <- NULL
  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    text <- c(
      text,
      clock_model_to_xml_operators(clock_model, i == 1)
    )
  }
  text
}
