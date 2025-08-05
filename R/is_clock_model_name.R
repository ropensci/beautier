#' Determines if the name is a valid clock model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid clock_model name, FALSE otherwise
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_clock_model_name("relaxed_log_normal")
#' is_clock_model_name("strict")
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
is_clock_model_name <- function(name) {
  name %in% beautier::get_clock_model_names()
}
