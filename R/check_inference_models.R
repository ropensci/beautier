#' Check if the \code{inference_model} is a valid BEAUti inference model.
#'
#' Calls \code{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso Use \link{create_inference_model} to create a valid
#'   BEAST2 options object
#' @examples
#' check_empty_beautier_folder()
#'
#' check_inference_models(list(create_inference_model()))
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
check_inference_models <- function(
  inference_models
) {
  if (!is.list(inference_models)) {
    stop("'inference_models' must be a list")
  }
  for (i in seq_along(inference_models)) {
    # Stub with too simple error message
    check_inference_model(inference_models[[i]])
  }
}
