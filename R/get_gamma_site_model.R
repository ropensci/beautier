#' Extract the gamma site model from an HKY site model
#' @param site_model one or more gamma_site_models, as created
#'   by \code{\link{create_gamma_site_model}}
#' @return the gamma site model
#' @author Richel J.C. Bilderbeek
#' @export
get_gamma_site_model <- function(site_model) {

  if (!beautier::is_site_model(site_model)) {
    stop("site_models must be a site_model")
  }
  testit::assert("gamma_site_model" %in% names(site_model))
  site_model$gamma_site_model
}
