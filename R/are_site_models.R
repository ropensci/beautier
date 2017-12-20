#' Determine if x consists out of site_models objects
#' @param x the object to check if it consists out of site_models objects
#' @return TRUE if x, or all elements of x, are site_model objects
#' @author Richel J.C. Bilderbeek
are_site_models <- function(
  x
) {
  if (is.null(x)) return(FALSE)
  if (is_site_model(x)) return(TRUE)
  for (i in x) {
    if (!is_site_model(i)) return(FALSE)
  }
  return(TRUE)
}
