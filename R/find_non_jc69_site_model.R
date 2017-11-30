#' Finds the first non-JC69 site model from a list of one or more site models
#' @param site_models a list of one or more site models,
#'   as returned by \code{\link{create_site_model}}
#' @return the first non-JC69 site model, NULL if such a site model is
#'   absent
#' @author Richel J.C. Bilderbeek
#' @export
find_non_jc69_site_model <- function(
  site_models
) {
  if (!beautier::are_site_models(site_models)) {
    stop("'site_models' must be a list of site models")
  }
  for (site_model in site_models) {
    if (!is_jc69_site_model(site_model)) {
      return(site_model)
    }
  }
  NULL
}
