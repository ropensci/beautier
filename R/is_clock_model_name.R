#' Determines if the name is a valid clock model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid clock_model name, FALSE otherwise
#' @export
is_clock_model_name <- function(name) {
  return(name %in% beautier::get_clock_model_names())
}
