#' Determines if the name is a valid clock model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid clock_model name, FALSE otherwise
#' @examples
#'   testit::assert(beautier:::is_clock_model_name("relaxed_log_normal"))
#'   testit::assert(beautier:::is_clock_model_name("strict"))
#' @author Richel J.C. Bilderbeek
is_clock_model_name <- function(name) {
  name %in% get_clock_model_names()
}
