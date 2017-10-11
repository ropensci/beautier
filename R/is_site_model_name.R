#' Determines if the name is a valid site_model name
#' @param name the name to be tested
#' @return TRUE if the name is a valid site_model name, FALSE otherwise
#' @export
is_site_model_name <- function(name) {
  return(name %in% "JC69")
}
