#' Determines if the name is a valid clock model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid clock_model name, FALSE otherwise
#' @examples
#' library(testthat)
#'
#' expect_true(is_clock_model_name("relaxed_log_normal"))
#' expect_true(is_clock_model_name("strict"))
#' @author Richèl J.C. Bilderbeek
#' @export
is_clock_model_name <- function(name) {
  name %in% beautier::get_clock_model_names()
}
