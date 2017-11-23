#' Determine if the object is a valid TN93 site model,
#'   as created by \code{\link{create_tn93_site_model}}
#' @param x an object, to be determined if it is a valid TN93 site model
#' @return TRUE if x is a valid TN93 site model, FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
is_tn93_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "TN93") return(FALSE)
  if (!"kappa_1_prior_distr" %in% names(x)) return(FALSE)
  if (!"kappa_2_prior_distr" %in% names(x)) return(FALSE)
  TRUE

}
