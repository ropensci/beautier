#' Extract the gamma shape from an gamma site model
#' @param gamma_site_model one or more gamma_site_models, as created
#'   by \code{\link{create_gamma_site_model}}
#' @return the gamma category count
#' @export
get_gamma_shape <- function(gamma_site_model) {

  if (!is_gamma_site_model(gamma_site_model)) {
    stop("site_models must be a gamma_site_model")
  }
  testit::assert("gamma_shape" %in% names(gamma_site_model))
  gamma_site_model$gamma_shape
}
