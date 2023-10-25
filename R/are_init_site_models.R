#' Determine if x consists out of initialized site_models objects
#' @param x the object to check if it consists out of
#'   initialized site_models objects
#' @return TRUE if x, or all elements of x, are initialized site_model objects
#' @author Richèl J.C. Bilderbeek
#' @export
are_init_site_models <- function(
  x
) {
  if (!are_site_models(x)) return(FALSE)
  for (i in x) {
    if (!is_init_site_model(i)) return(FALSE)
  }
  return(TRUE)
}
