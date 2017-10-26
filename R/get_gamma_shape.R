#' Extract the gamma shape from an gamma site model
#' @param gamma_site_models one or more gamma_site_models, as created
#'   by \code{\link{create_gamma_site_model}}
#' @return the gamma category count
#' @export
get_gamma_shape <- function(gamma_site_models) {

  if (!is_gamma_site_model(gamma_site_models)) {
    stop("site_models must be one or more gamma_site_models")
  }
  testit::assert("gamma_shape" %in% names(gamma_site_models))
  gamma_site_models$gamma_shape
}

#' Get the default gamma shape value
#' @note this value is returned as a string, to be able to exactly replicate
#'   all XML files
#' @export
get_default_gamma_shape <- function() {
  return("1.0")
}
