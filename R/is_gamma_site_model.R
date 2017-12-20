#' Is object x a gamma site model?
#' @param x the object to be determined if it is a valid gamma site object
#' @return TRUE if x is a valid gamma site object, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'   gamma_site_model <- create_gamma_site_model()
#'   testit::assert(beautier:::is_gamma_site_model(gamma_site_model))
is_gamma_site_model <- function(x) {

  if (!"gamma_cat_count" %in% names(x)) return(FALSE)
  if (x$gamma_cat_count < 0) return(FALSE)
  if (!"gamma_shape" %in% names(x)) return(FALSE)
  if (x$gamma_shape < 0) return(FALSE)
  if (!"prop_invariant" %in% names(x)) return(FALSE)
  if (x$prop_invariant < 0.0) return(FALSE)
  if (x$prop_invariant > 1.0) return(FALSE)
  if (!"gamma_shape_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$gamma_shape_prior_distr)) return(FALSE)
  TRUE
}
