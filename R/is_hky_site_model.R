#' Determine if the object is a valid HKY site model,
#' as created by \code{\link{create_hky_site_model}}
#' @param x an object, to be determined if it is a valid HKY site model
#' @return TRUE if x is a valid HKY site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_hky_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "HKY") return(FALSE)
  if (!"kappa_prior_distr" %in% names(x)) return(FALSE)
  TRUE
}
