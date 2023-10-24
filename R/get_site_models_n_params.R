#' Get the number of distributions one or more site models have
#' @inheritParams default_params_doc
#' @return the number of parameters the site models have
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # Ten
#' get_site_models_n_params(list(create_gtr_site_model()))
#'
#' # Two
#' get_site_models_n_params(list(create_hky_site_model()))
#'
#' # Zero
#' get_site_models_n_params(list(create_jc69_site_model()))
#'
#' # Four
#' get_site_models_n_params(list(create_tn93_site_model()))
#'
#' check_empty_beautier_folder()
#' @export
get_site_models_n_params <- function(
  site_models
) {
  if (!are_site_models(site_models)) {
    stop("'site_models' must be a list of site models")
  }
  n <- 0
  for (site_model in site_models) {
    check_true(is_site_model(site_model))
    n <- n + get_site_model_n_params(site_model)
  }
  n
}
