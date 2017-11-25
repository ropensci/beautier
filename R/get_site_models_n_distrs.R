#' Get the number of distributions a site model has
#' @param site_models a list of one or more site_models,
#'   each created by \code{\link{create_site_model}}
#' @return the number of distributions the site models have
#' @author Richel J.C. Bilderbeek
#' @export
get_site_models_n_distrs <- function(
  site_models
) {
  if (!beautier::are_site_models(site_models)) {
    stop("'site_models' must be a list of site models")
  }
  n <- 0
  for (site_model in site_models) {
    testit::assert(beautier::is_site_model(site_model))
    n <- n + beautier::get_site_model_n_distrs(site_model)
  }
  n
}
