#' Get the clock model names
#' @return the clock model names
#' @seealso Use \link{create_clock_models} to create a list
#'   with all clock models
#' @examples
#'   names <- beautier:::get_clock_model_names()
#'   testit::assert("relaxed_log_normal" %in% names)
#'   testit::assert("strict" %in% names)
#' @author Richèl J.C. Bilderbeek
#' @export
get_clock_model_names <- function() {
  c("relaxed_log_normal", "strict")
}
