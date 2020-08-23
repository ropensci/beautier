#' Get the clock model names
#' @return the clock model names
#' @seealso Use \link{create_clock_models} to create a list
#'   with all clock models
#' @examples
#' names <- get_clock_model_names()
#' @author Richèl J.C. Bilderbeek
#' @export
get_clock_model_names <- function() {
  c("relaxed_log_normal", "strict")
}
