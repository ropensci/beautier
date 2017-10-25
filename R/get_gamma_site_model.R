#' Extract the gamma site model from an HKY site model
#' @param site_models one or more gamma_site_models, as created
#'   by \code{\link{create_gamma_site_model}}
#' @return the gamma site model
#' @export
get_gamma_site_model <- function(site_models) {

  if (!is_site_model(site_models)) {
    stop("site_models must be one or more site_models")
  }
  testit::assert("gamma_site_model" %in% names(site_models))
  site_models$gamma_site_model
}
