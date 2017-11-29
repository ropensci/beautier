#' Extract the kappa from an HKY site model
#' @param site_models one or more site_models
#' @return the kappa
#' @export
get_kappa <- function(site_models) {

  if (!beautier::is_site_model(site_models)) {
    stop("site_models must be one or more site_models")
  }
  testit::assert("kappa" %in% names(site_models))
  site_models$kappa
}
