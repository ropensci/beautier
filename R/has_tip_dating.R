#' Determine if the \code{inference_model} uses tip dating.
#'
#' Determine if the \code{inference_model} uses tip dating
#' @inheritParams default_params_doc
#' @return TRUE if the \code{inference_model} uses tip dating,
#' FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Yes, has tip dating
#' has_strict_clock_model(
#'   create_inference_model(
#'     tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv")
#'   )
#' )
#'
#' # No tip dating
#' has_strict_clock_model(
#'   create_inference_model()
#' )
#' @export
has_tip_dating <- function(inference_model) {
  beautier::check_inference_model(inference_model)
  !beautier::is_one_na(inference_model$tipdates_filename)
}
