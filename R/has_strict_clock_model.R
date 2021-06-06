#' Determine if the \code{inference_model} uses a strict clock model.
#'
#' Determine if the \code{inference_model} uses a strict clock model
#' @inheritParams default_params_doc
#' @return TRUE if the \code{inference_model} uses a strict clock model,
#' FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Yes, has a strict clock model
#' has_strict_clock_model(
#'   create_inference_model(clock_model = create_strict_clock_model())
#' )
#'
#' # No strict clock model
#' has_strict_clock_model(
#'   create_inference_model(clock_model = create_rln_clock_model())
#' )
#' @export
has_strict_clock_model <- function(inference_model) {
  beautier::check_inference_model(inference_model)
  beautier::is_strict_clock_model(inference_model$clock_model)
}
