#' Create clock models from their names
#' @inheritParams default_params_doc
#' @examples
#'   names <- get_clock_model_names()
#'   clock_models <- create_clock_models_from_names(names)
#'
#'   for (i in seq_along(names)) {
#'     testthat::expect_equal(names[i], clock_models[[i]]$name)
#'   }
#' @author Richel J.C. Bilderbeek
#' @export
create_clock_models_from_names <- function(clock_model_names) {
  clock_models <- list()
  for (i in seq_along(clock_model_names)) {
    clock_model_name <- clock_model_names[i]
    clock_models[[i]] <- create_clock_model_from_name(clock_model_name) # nolint beautier function
  }
  clock_models
}
