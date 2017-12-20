#' Get the first clock model of each ID
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
get_unlinked_clock_models <- function(clock_models) {
  testit::assert(are_clock_models(clock_models))
  results <- list()
  ids <- NULL
  for (clock_model in clock_models) {
    id <- clock_model$id
    if (!id %in% ids) {
      ids <- c(ids, id)
      results[[length(ids)]] <- clock_model
    }
  }

  testit::assert(are_clock_models(results))
  results
}
