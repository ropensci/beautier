#' Finds a site model with a certain ID
#' @inheritParams default_params_doc
#' @param id the ID of the site model
#' @return the site models with the desired ID, NULL if such a site model is
#'   absent
#' @author Richel J.C. Bilderbeek
#' @export
find_site_model <- function(
  site_models,
  id
) {
  if (!beautier::are_site_models(site_models)) {
    stop("'site_models' must be a list of site models")
  }
  if (!beautier::is_id(id)) {
    stop("'id' must be an ID")
  }
  for (site_model in site_models) {
    if (site_model$id == id) {
      return(site_model)
    }
  }
  NULL
}
