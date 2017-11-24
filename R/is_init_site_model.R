#' Determine if x is an initialized site_model object,
#' as created by \code{\link{create_site_model}}
#' @param x the object to check if it is an
#'   initialized site_models object
#' @return TRUE if x is an initialized site_model object
#' @author Richel J.C. Bilderbeek
is_init_site_model <- function(
  x
) {
  if (!beautier::is_site_model(x)) return(FALSE)
  if (beautier::is_gtr_site_model(x)) {
    return(TRUE)
  } else if (beautier::is_hky_site_model(x)) {
    return(is_init_hky_site_model(x)) # nolint internal function call
  } else if (beautier::is_jc69_site_model(x)) {
    return(TRUE)
  } else {
    testit::assert(beautier::is_tn93_site_model(x))
    return(is_init_tn93_site_model(x)) # nolint internal function call
  }
}


#' Determine if x is an initialized hky site_model object
#' as created by \code{\link{create_hky_site_model}}
#' @param x the object to check if it is an
#'   initialized HKY site model object
#' @return TRUE if x is an initialized HKY site_model object
#' @author Richel J.C. Bilderbeek
#' @export
is_init_hky_site_model <- function(
  x
) {
  testit::assert(beautier::is_hky_site_model(x))
  is_init_distr(x$kappa_prior) # nolint internal function
}

#' Determine if x is an initialized tn93 site_model object
#' as created by \code{\link{create_tn93_site_model}}
#' @param x the object to check if it is an
#'   initialized TN93 site model object
#' @return TRUE if x is an initialized TN93 site model object
#' @author Richel J.C. Bilderbeek
#' @export
is_init_tn93_site_model <- function(
  x
) {
  testit::assert(beautier::is_tn93_site_model(x))
  is_init_distr(x$kappa_1_prior) &&
    is_init_distr(x$kappa_2_prior)
}
