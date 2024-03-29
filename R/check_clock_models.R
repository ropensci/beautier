#' Check if the object is a list of one or more clock models.
#'
#' Will \link{stop} if the object is not a list of one or more clock models.
#' @param clock_models the object to be checked if it is a list of one
#'   or more valid clock models
#' @return nothing.
#'   Will \link{stop} if the object is not a list of one or more clock models.
#' @seealso Use \link{create_clock_model} to create a valid clock model
#' @examples
#' check_empty_beautier_folder()
#'
#' check_clock_models(create_strict_clock_model())
#' check_clock_models(list(create_strict_clock_model()))
#' check_clock_models(
#'   list(create_strict_clock_model(), create_rln_clock_model())
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_clock_models <- function(clock_models) {

  if (!is.list(clock_models)) {
    stop(
      "'clock_models' must be a list of one or more valid clock models. \n",
      "Actual value: ", clock_models
    )
  }

  if (is_clock_model(clock_models)) {
    clock_models <- list(clock_models)
  }
  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    tryCatch(
      check_clock_model(clock_model),
      error = function(e) {
        stop(
          "'clock_models' must be a list of one or more valid clock models. \n",
          "Error in clock_models[[", i, "]]: ", e$message, ". \n",
          "Actual value: ", clock_model
        )
      }
    )
  }
}
