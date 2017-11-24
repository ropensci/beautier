#' Determines if the name is a valid parameter name
#' @param name the name to be tested
#' @return TRUE if the name is a valid parameter name, FALSE otherwise
#' @export
isparam_name <- function(name) {
  return(name %in% beautier::getparam_names())
}
