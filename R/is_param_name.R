#' Determines if the name is a valid parameter name
#' @param name the name to be tested
#' @return TRUE if the name is a valid parameter name, FALSE otherwise
#' @author Richel J.C. Bilderbeek
is_param_name <- function(name) {
  name %in% beautier::get_param_names()
}
