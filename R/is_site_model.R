#' Determine if the object is a valid site_model
#' @param x an object, to be determined if it is a site_model
#' @return TRUE if the site_model is a valid site_model, FALSE otherwise
#' @export
is_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) {
    return(FALSE)
  }
  if (!is_site_model_name(x$name)) {
    return(FALSE)
  }
  if (!"gamma_site_model" %in% names(x)) {
    return(FALSE)
  }
  return(TRUE)
}
