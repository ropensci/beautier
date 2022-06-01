#' Get the clock model names
#' @return the clock model names
#' @seealso Use \link{create_clock_models} to create a list
#'   with all clock models
#' @examples
#' check_empty_beautier_folder()
#'
#' get_clock_model_names()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
get_clock_model_names <- function() {
  c("relaxed_log_normal", "strict")
}
