#' Get the index of a clock model with a list of clock models
#' @inheritParams default_params_doc
#' @return the index of the first clock models with the same ID,
#'   NULL if such a clock model is absent
#' @author Richel J.C. Bilderbeek
#' @examples
#'   a <- create_strict_clock_model(id = 1)
#'   b <- create_rln_clock_model(id = 2)
#'   ab <- list(a, b)
#'   testit::assert(beautier:::get_first_clock_model_index(a, ab) == 1)
#'   testit::assert(beautier:::get_first_clock_model_index(b, ab) == 2)
get_first_clock_model_index <- function(
  clock_model,
  clock_models
) {
  if (!is_clock_model(clock_model)) {
    stop("'clock_model' must be a clock model")
  }
  if (!are_clock_models(clock_models)) {
    stop("'clock_models' must be a list of clock models")
  }
  for (i in seq_along(clock_models)) {
    if (clock_models[[i]]$id == clock_model$id) {
      return(i)
    }
  }
  NULL
}
