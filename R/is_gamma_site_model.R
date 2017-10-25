#' Is object x a gamma site model?
#' @param x the object to be determined if it is a valid gamma site object
#' @return TRUE if x is a valid gamma site object, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_gamma_site_model <- function(x) {

  if (!"gamma_cat_count" %in% names(x)) {
    return(FALSE)
  }
  if (!"prop_invariant" %in% names(x)) {
    return(FALSE)
  }
  TRUE
}
