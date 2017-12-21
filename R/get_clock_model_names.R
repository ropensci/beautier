#' Get the clock model names
#' @return the clock model names
#' @examples
#'   names <- beautier:::get_clock_model_names()
#'   testit::assert("relaxed_log_normal" %in% names)
#'   testit::assert("strict" %in% names)
#' @author Richel J.C. Bilderbeek
get_clock_model_names <- function() {
  c("relaxed_log_normal", "strict")
}
