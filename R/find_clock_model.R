#' Finds a clock model with a certain ID
#' @inheritParams default_params_doc
#' @param id the ID of the clock model
#' @return the clock models with the desired ID, NULL if such a clock model is
#'   absent
#' @author Richèl J.C. Bilderbeek
#' @export
find_clock_model <- function(
  clock_models,
  id
) {
  if (!beautier::are_clock_models(clock_models)) {
    stop("'clock_models' must be a list of clock models")
  }
  if (!beautier::is_id(id)) {
    stop("'id' must be an ID")
  }
  for (clock_model in clock_models) {
    if (clock_model$id == id) {
      return(clock_model)
    }
  }
  NULL
}
