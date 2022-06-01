#' Get the number of distributions a site model has
#' @inheritParams default_params_doc
#' @return the number of distributions the site models have
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # 5
#' get_site_models_n_distrs(list(create_gtr_site_model()))
#' # 1
#' get_site_models_n_distrs(list(create_hky_site_model()))
#' # 0
#' get_site_models_n_distrs(list(create_jc69_site_model()))
#' # 2
#' get_site_models_n_distrs(list(create_tn93_site_model()))
#'
#' check_empty_beautier_folder()
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
