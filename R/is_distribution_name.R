#' Determines if the name is a valid distribution name
#' @param name the name to be tested
#' @return TRUE if the name is a valid distribution name, FALSE otherwise
#' @export
is_distr_name <- function(name) {
  return(name %in% beautier::get_distr_names())
}
