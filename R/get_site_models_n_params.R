#' Get the number of distributions one or more site models have
#' @inheritParams default_params_doc
#' @return the number of parameters the site models have
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   testit::assert(
#'     beautier:::get_site_models_n_params(list(create_gtr_site_model())) == 11
#'   )
#'   testit::assert(
#'     beautier:::get_site_models_n_params(list(create_hky_site_model())) == 3
#'   )
#'   testit::assert(
#'     beautier:::get_site_models_n_params(list(create_jc69_site_model())) == 1
#'   )
#'   testit::assert(
#'     beautier:::get_site_models_n_params(list(create_tn93_site_model())) == 5
#'   )
#' @noRd
get_site_models_n_params <- function(
  site_models
) {
  if (!are_site_models(site_models)) { # nolint beautier function
    stop("'site_models' must be a list of site models")
  }
  n <- 0
  for (site_model in site_models) {
    testit::assert(is_site_model(site_model)) # nolint beautier function
    n <- n + get_site_model_n_params(site_model) # nolint beautier function
  }
  n
}
