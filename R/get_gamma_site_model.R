#' Extract the gamma site model from a site model
#' @inheritParams default_params_doc
#' @return the gamma site model
#' @author Richel J.C. Bilderbeek
#' @export
get_gamma_site_model <- function(site_model) {

  if (!is_site_model(site_model)) {
    stop("site_models must be a site_model")
  }
  testit::assert("gamma_site_model" %in% names(site_model))
  site_model$gamma_site_model
}
