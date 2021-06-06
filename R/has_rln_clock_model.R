#' Determine if the \code{inference_model} uses
#' a relaxed log-normal clock model.
#'
#' Determine if the \code{inference_model} uses
#' a relaxed log-normal clock model.
#' @inheritParams default_params_doc
#' @return TRUE if the \code{inference_model} uses
#' a relaxed log-normal clock model,
#' FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Yes, has a RLN clock model
#' has_rln_clock_model(
#'   create_inference_model(clock_model = create_rln_clock_model())
#' )
#'
#' # No RLN clock model
#' has_rln_clock_model(
#'   create_inference_model(clock_model = create_strict_clock_model())
#' )
#' @export
has_rln_clock_model <- function(inference_model) {
  beautier::check_inference_model(inference_model)
  beautier::is_rln_clock_model(inference_model$clock_model)
}
