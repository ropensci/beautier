#' Create clock models from their names
#' @inheritParams default_params_doc
#' @return a list of one or more clock models
#' @seealso Use \link{create_clock_models} to get all clock models
#' @examples
#' check_empty_beautier_folder()
#'
#' create_clock_models_from_names(get_clock_model_names())
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_clock_models_from_names <- function(clock_model_names) {
  clock_models <- list()
  for (i in seq_along(clock_model_names)) {
    clock_model_name <- clock_model_names[i]
    clock_models[[i]] <- beautier::create_clock_model_from_name(
      clock_model_name
    )
  }
  clock_models
}
