#' Determine if the object is a valid HKY site model,
#' as created by \code{\link{create_hky_site_model}}
#' @param x an object, to be determined if it is a valid HKY site model
#' @return TRUE if x is a valid HKY site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @examples
#'   hky_site_model <- create_hky_site_model()
#'   testit::assert(beautier:::is_hky_site_model(hky_site_model))
is_hky_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "HKY") return(FALSE)
  if (!"kappa" %in% names(x)) return(FALSE)
  if (!"kappa_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$kappa_prior_distr)) return(FALSE)
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE
}
