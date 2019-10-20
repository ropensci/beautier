#' Get the number of distributions one or more site models have
#' @inheritParams default_params_doc
#' @return the number of parameters the site models have
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     get_site_models_n_params(list(create_gtr_site_model())) == 10
#'   )
#'   testit::assert(
#'     get_site_models_n_params(list(create_hky_site_model())) == 2
#'   )
#'   testit::assert(
#'     get_site_models_n_params(list(create_jc69_site_model())) == 0
#'   )
#'   testit::assert(
#'     get_site_models_n_params(list(create_tn93_site_model())) == 4
#'   )
#' @export
get_site_models_n_params <- function(
  site_models
) {
  if (!beautier::are_site_models(site_models)) {
    stop("'site_models' must be a list of site models")
  }
  n <- 0
  for (site_model in site_models) {
    testit::assert(beautier::is_site_model(site_model))
    n <- n + beautier::get_site_model_n_params(site_model)
  }
  n
}
