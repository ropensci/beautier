#' Is the index the first of a series of clock models that are shared?
#' @inheritParams default_params_doc
#' @param i the index of the clock model within \code{clock_models}
#' @author Richel J.C. Bilderbeek
is_index_of_first_shared_clock_model <- function( # nolint internal function
  i,
  clock_models
) {
  testit::assert(i >= 1)
  testit::assert(i <= length(clock_models))
  testit::assert(are_clock_models(clock_models))
  if (length(clock_models) <= 1) return(FALSE)
  ids <- get_clock_models_ids(clock_models) # nolint internal function
  id <- clock_models[[i]]$id
  n_clock_models <- length(clock_models)
  ids <- ids[i + 1:n_clock_models]
  id %in% ids
}
