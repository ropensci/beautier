#' Get the number of distributions one or more site models have
#' @param site_models a list of site_models,
#'   as created by \code{\link{create_site_model}}
#' @return the number of parameters the site models have
#' @author Richel J.C. Bilderbeek
#' @export
get_site_models_n_params <- function(
  site_models
) {
  if (!are_site_models(site_models)) {
    stop("'site_models' must be a list of site models")
  }
  n <- 0
  for (site_model in site_models) {
    testit::assert(is_site_model(site_model))
    n <- n + get_site_model_n_params(site_model)
  }
  n
}

