#' Collect the IDs of the list of clock models
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
get_clock_models_ids <- function(
  clock_models
) {
  testit::assert(are_clock_models(clock_models))
  n <- length(clock_models)
  ids <- rep(NA, n)
  for (i in seq_along(clock_models)) {
    ids[i] <- clock_models[[i]]$id
  }
  ids
}
