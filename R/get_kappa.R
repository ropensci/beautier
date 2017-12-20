#' Extract the kappa from an HKY site model
#' @inheritParams default_params_doc
#' @return the kappa
#' @author Richel J.C. Bilderbeek
#' @export
get_kappa <- function(site_models) {

  if (!is_site_model(site_models)) {
    stop("site_models must be one or more site_models")
  }
  testit::assert("kappa" %in% names(site_models))
  site_models$kappa
}
