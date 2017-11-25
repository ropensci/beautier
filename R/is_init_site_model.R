#' Determine if x is an initialized site model,
#' as created by \code{\link{create_site_model}}
#' @param x the object to check if it is an
#'   initialized site_models object
#' @return TRUE if x is an initialized site model
#' @author Richel J.C. Bilderbeek
is_init_site_model <- function(
  x
) {
  if (!beautier::is_site_model(x)) return(FALSE)
  if (beautier::is_gtr_site_model(x)) {
    return(is_init_gtr_site_model(x)) # nolint internal function call
  } else if (beautier::is_hky_site_model(x)) {
    return(is_init_hky_site_model(x)) # nolint internal function call
  } else if (beautier::is_jc69_site_model(x)) {
    return(TRUE)
  } else {
    testit::assert(beautier::is_tn93_site_model(x))
    return(is_init_tn93_site_model(x)) # nolint internal function call
  }
}

#' Determine if x is an initialized GTR site model
#' as created by \code{\link{create_gtr_site_model}}
#' @param x the object to check if it is an
#'   initialized HKY site model
#' @return TRUE if x is an initialized GTR site model
#' @author Richel J.C. Bilderbeek
is_init_gtr_site_model <- function(
  x
) {
  testit::assert(beautier::is_gtr_site_model(x))
  if (is.na(x$gamma_0_alpha$id)) return(FALSE)
  if (is.na(x$gamma_0_beta$id)) return(FALSE)
  if (is.na(x$gamma_1_alpha$id)) return(FALSE)
  if (is.na(x$gamma_1_beta$id)) return(FALSE)
  if (is.na(x$gamma_2_alpha$id)) return(FALSE)
  if (is.na(x$gamma_2_beta$id)) return(FALSE)
  if (is.na(x$gamma_3_alpha$id)) return(FALSE)
  if (is.na(x$gamma_3_beta$id)) return(FALSE)
  TRUE
}

#' Determine if x is an initialized hky site model
#' as created by \code{\link{create_hky_site_model}}
#' @param x the object to check if it is an
#'   initialized HKY site model
#' @return TRUE if x is an initialized HKY site model
#' @author Richel J.C. Bilderbeek
is_init_hky_site_model <- function(
  x
) {
  testit::assert(beautier::is_hky_site_model(x))
  is_init_distr(x$kappa_prior) # nolint internal function
}

#' Determine if x is an initialized tn93 site model
#' as created by \code{\link{create_tn93_site_model}}
#' @param x the object to check if it is an
#'   initialized TN93 site model
#' @return TRUE if x is an initialized TN93 site model
#' @author Richel J.C. Bilderbeek
is_init_tn93_site_model <- function(
  x
) {
  testit::assert(beautier::is_tn93_site_model(x))
  is_init_distr(x$kappa_1_prior) &&
    is_init_distr(x$kappa_2_prior)
}
