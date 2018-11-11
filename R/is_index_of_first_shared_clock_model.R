#' Is the index the first of a series of clock models that are shared?
#' @inheritParams default_params_doc
#' @param i the index of the clock model within \code{clock_models}
#' @author Richel J.C. Bilderbeek
#' @noRd
is_index_of_first_shared_clock_model <- function( # nolint internal function
  i,
  clock_models
) {
  testit::assert(i >= 1)
  testit::assert(i <= length(clock_models))
  testit::assert(are_clock_models(clock_models)) # nolint internal function
  if (length(clock_models) <= 1) return(FALSE)
  # Collect all IDS
  ids <- get_clock_models_ids(clock_models) # nolint internal function
  # Get our ID
  id <- clock_models[[i]]$id
  testit::assert(sum(id == ids) >= 1)
  if (sum(id == ids) == 1) return(FALSE)
  if (i == 1) return(TRUE)
  # Keep the IDs before the ID (-1 to exclude that ID)
  ids <- ids[1:(i - 1)]
  !id %in% ids
}
