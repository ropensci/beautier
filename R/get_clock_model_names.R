#' Get the clock model names
#' @return the clock model names
#' @examples
#'   names <- get_clock_model_names()
#'   testthat::expect_true("relaxed_log_normal" %in% names)
#'   testthat::expect_true("strict" %in% names)
#' @author Richel J.C. Bilderbeek
#' @export
get_clock_model_names <- function() {
  return(c("relaxed_log_normal", "strict"))
}
