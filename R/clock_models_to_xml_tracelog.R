clock_models_to_xml_tracelog <- function(
  clock_models
) {
  testit::assert(beautier::are_clock_models(clock_models))
  text <- NULL
  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))
    text <- c(text, clock_model_to_xml_tracelog(clock_model, i == 1))
  }
  text
}
