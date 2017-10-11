#' Create a random alignment
#' @param site_model a site_model, as created by create_site_model
#' @return TRUE if the site_model is a valid site_model, FALSE otherwise
#' @export
is_valid_site_model <- function(
  site_model
) {
  if (!is_site_model_name(site_model$name))
  {
    return(FALSE)
  }
  return(TRUE)
}
