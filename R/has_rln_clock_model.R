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
#' if (is_on_ci()) {
#'   check_empty_beautier_folder()
#'
#'   # Yes, has a RLN clock model
#'   has_rln_clock_model(
#'     create_inference_model(clock_model = create_rln_clock_model())
#'   )
#'
#'   # No RLN clock model
#'   has_rln_clock_model(
#'     create_inference_model(clock_model = create_strict_clock_model())
#'   )
#'
#'   check_empty_beautier_folder()
#' }
#' @export
has_rln_clock_model <- function(inference_model) {
  check_inference_model(inference_model)
  is_rln_clock_model(inference_model$clock_model)
}
